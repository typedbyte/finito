{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AI.Search.FiniteDomain.Int.Constraint
  ( (#=)
  , (#/=)
  , (#<)
  , (#<=)
  , (#>)
  , (#>=)
  , (/\)
  , (\/)
  , Constraint
  , FD
  , Labeling(..)
  , allDifferent
  , between
  , initNewVar
  , labeling
  , newVar
  , not'
  , runFD
  ) where

-- base
import Control.Monad    ( forM, forM_ )
import Control.Monad.ST ( ST, runST )
import Data.List        ( find )

-- domain
import qualified Numeric.Domain as D

-- propeller
import Data.Propagator.Cell as P ( Cell, cell, connect, label, propagateMany
                                 , readCell, succeeded, sync, syncWith )

-- transformers
import Control.Monad.Trans.State ( State, evalState, execState, get, modify, put )

import AI.Search.FiniteDomain.Int.Cell       ( domainJoin, eqJoin, mustHoldJoin )
import AI.Search.FiniteDomain.Int.Expression ( Expression, cellifyExpression, var )

-- | All constraint solving actions are peformed in the FD monad which tracks
-- created variables and specified constraints.
newtype FD a = FD { unFD :: State ([IntConstraint], Int) a }
  deriving (Applicative, Functor, Monad)

-- | A constraint restricts the possible values of an involved 'Expression'.
type Constraint = FD ()

addConstraint :: IntConstraint -> FD ()
addConstraint cons = FD $ 
  modify (\(cs, ix) -> (cons : cs, ix))

-- | Runs the FD monad computation.
runFD :: FD a -> a
runFD (FD state) = evalState state ([], 0)

data IntConstraint
  = Equals    Expression Expression
  | NotEquals Expression Expression
  | LessThan  Expression Expression
  | And       IntConstraint IntConstraint
  | Or        IntConstraint IntConstraint
  deriving (Eq, Ord, Show)

-- | Enforces that two expressions have the same value.
infix 4 #=
(#=) :: Expression -> Expression -> Constraint
(#=) left right = addConstraint $ left `Equals` right

-- | Enforces that two expressions have different values.
infix 4 #/=
(#/=) :: Expression -> Expression -> Constraint
(#/=) left right = addConstraint $ left `NotEquals` right

-- | Enforces that the value of an expression is less than the value of another
-- expression.
infix 4 #<
(#<) :: Expression -> Expression -> Constraint
(#<) left right = addConstraint $ left `LessThan` right

-- | Enforces that the value of an expression is less than or equal to the
-- value of another expression.
infix 4 #<=
(#<=) :: Expression -> Expression -> Constraint
(#<=) left right =
  addConstraint $ (left `LessThan` right) `Or` (left `Equals` right)

-- | Enforces that the value of an expression is greater than the value of
-- another expression.
infix 4 #>
(#>) :: Expression -> Expression -> Constraint
(#>) = flip (#<)

-- | Enforces that the value of an expression is greater than or equal to
-- the value of another expression.
infix 4 #>=
(#>=) :: Expression -> Expression -> Constraint
(#>=) = flip (#<=)

-- | Conjunction of constraints, i.e. both constraints must hold.
infixl 3 /\
(/\) :: Constraint -> Constraint -> Constraint
(/\) = (>>)

-- | Disjunction of constraints, i.e. at least one of the two constraints must
-- hold.
infixl 2 \/
(\/) :: Constraint -> Constraint -> Constraint
(\/) left right = FD $ do
  (cs, ix) <- get
  let (lcs, lx) = execState (unFD left) ([], ix)
      (rcs, nx) = execState (unFD right) ([], lx)
  case (lcs, rcs) of
    (  [],    _) -> put (cs ++ rcs, nx)
    (   _,   []) -> put (cs ++ lcs, nx)
    (l:ls, r:rs) ->
      let leftAnd    = foldl And l ls
          rightAnd   = foldl And r rs
          constraint = leftAnd `Or` rightAnd
      in put (constraint : cs, nx)

-- | Negates a constraint, i.e. the specified constraint must not hold.
not' :: Constraint -> Constraint
not' constraint = FD $ do
  (cons, ix) <- get
  let (ncs, nx) = execState (unFD constraint) ([], ix)
  case ncs of
    []   -> put (cons, nx)
    c:cs -> put (recNot (foldl And c cs) : cons, nx)
  where
    recNot (Equals l r)    = NotEquals l r
    recNot (NotEquals l r) = Equals l r
    recNot (LessThan l r)  = (r `LessThan` l) `Or` (r `Equals` l)
    recNot (And l r)       = Or (recNot l) (recNot r)
    recNot (Or l r)        = And (recNot l) (recNot r)

-- | Enforces that the all the given expressions have different values.
allDifferent :: [Expression] -> Constraint
allDifferent []     = pure ()
allDifferent (c:cs) = do
  sequence_ (fmap (c #/=) cs)
  allDifferent cs

-- | Enforces that the value of an expression lies within two bounds.
between :: Expression -- ^ The inclusive lower bound of the expression.
        -> Expression -- ^ The inclusive upper bound of the expression.
        -> Expression -- ^ The expression to be constrained.
        -> Constraint -- ^ The resulting constraint.
between low high target = do
  target #>= low
  target #<= high

-- | Creates a new variable with a default value range from negative infinity
-- to positive infinity. Values are assigned to variables during 'labeling'.
newVar :: FD Expression
newVar = FD $ do
  (cs, iD) <- get
  put (cs, iD + 1)
  pure (var iD)

-- | Creates a new variable and initializes it with a specific value.
initNewVar :: Expression -> FD Expression
initNewVar initExpr = do
  v <- newVar
  v #= initExpr
  pure v

-- | A labeling is the result of the solver when trying to assign values to
-- variables according to the given constraints. Solutions have the same
-- ordering as the expressions specified during 'labeling', so operations like
-- 'zip' can be used to relate expressions to their solution values.
data Labeling a
  = Unsolvable [D.Domain Int]
  -- ^ Indicates that the given set of constraints cannot be solved, i.e.
  --   there is no combination of values for the labelled variables to fulfil
  --   all the constraints. The provided list contains the values that were
  --   narrowed down during the search.
  | Unbounded  [D.Domain Int]
  -- ^ Indicates that the given set of constraints cannot be solved in its
  --   current form because at least one variable has a lower bound of negative
  --   infinity or an upper bound of positive infinity (i.e., potential
  --   solutions cannot be enumerated). The provided list contains the values
  --   that were narrowed down during the search.
  | Solutions  [a]
  -- ^ Indicates a successful assignment of values to the labelled variables.
  --   The list contains all possible solutions.
  deriving (Eq, Show)

instance Functor Labeling where
  fmap _ (Unsolvable ds) = Unsolvable ds
  fmap _ (Unbounded  ds) = Unbounded ds
  fmap f (Solutions  xs) = Solutions (fmap f xs)

instance Applicative Labeling where
  pure a = Solutions [a]
  Unsolvable ds <*> _             = Unsolvable ds
  Unbounded ds  <*> _             = Unbounded ds
  _             <*> Unsolvable ds = Unsolvable ds
  _             <*> Unbounded ds  = Unbounded ds
  Solutions f   <*> Solutions a   = Solutions (f <*> a)

instance Monad Labeling where
  Unsolvable ds >>= _ = Unsolvable ds
  Unbounded  ds >>= _ = Unbounded ds
  Solutions  xs >>= f = go [] xs
    where
      go acc []     = Solutions acc
      go acc (y:ys) =
        case f y of
          Unsolvable ds -> Unsolvable ds
          Unbounded  ds -> Unbounded ds
          Solutions  bs -> go (acc ++ bs) ys

-- | Searches all combinations of values for the given expressions (variables,
-- most likely) which fulfil all the constraints defined in the FD monad.
--
-- The result list has the same ordering as the expressions, so operations like
-- 'zip' are possible to relate the given expressions to their solution values.
labeling :: [Expression] -> FD (Labeling [Int])
labeling vars = do
  cons <- FD (fmap fst get)
  pure $
    runST $ do
      (res, rvs, cells) <- cellifyConstraints cons []
      trueCell <- cell True mustHoldJoin
      allCell  <- cell D.maxDomain domainJoin
      forM_ res $ \c -> connect c trueCell pure
      let userCells = fmap snd userView
          userView  =
            flip fmap vars $ \v -> do
              case find ((== v) . fst) rvs of
                Just av -> av
                Nothing -> (v, allCell)
      propagation <- propagateMany cells
      snapshot    <- forM userCells P.readCell
      if not (succeeded propagation) then
        pure (Unsolvable snapshot)
      else do
        if any D.isInfinite snapshot then
          pure (Unbounded snapshot)
        else do
          result <- label (concat . D.elems) D.singleton userCells
          case result of
            [] -> pure (Unsolvable snapshot)
            xs -> pure (Solutions xs)

type DomainCell s = Cell s (D.Domain Int)
type LogicCell s  = Cell s Bool
type VarCell s    = (Expression, DomainCell s)

-- | Converts constraints to propagator cells.
-- The result consists of the new cells that represent the constraints, a list
-- of currently declared variables, and a list of all cells that were created
-- for the constraints.
cellifyConstraints
  :: [IntConstraint]
  -> [VarCell s]
  -> ST s ([LogicCell s], [VarCell s], [DomainCell s])
cellifyConstraints cons vars =
  case cons of
    [] -> pure ([], vars, [])
    c:cs -> do
      (ls, nvs, xs) <- cellifyConstraint c vars
      (rs, rvs, ys) <- cellifyConstraints cs nvs
      pure (ls : rs, rvs, xs ++ ys)

-- | Converts a constraint to a propagator cell.
-- The result consists of the new cell that represents the constraint, a list
-- of currently declared variables, and a list of all cells that were created
-- for this constraint.
cellifyConstraint
  :: IntConstraint
  -> [VarCell s]
  -> ST s (LogicCell s, [VarCell s], [DomainCell s])
cellifyConstraint constraint vars =
  case constraint of
    Equals left right ->
      binary left right sync
    NotEquals left right ->
      binary left right (syncWith D.notEqual D.notEqual)
    LessThan left right ->
      binary left right (syncWith D.greaterThanDomain D.lessThanDomain)
    And left right -> do
      (ls, nvs, xs) <- cellifyConstraint left vars
      (rs, rvs, ys) <- cellifyConstraint right nvs
      newCell <- cell True eqJoin
      connect ls newCell (\ld -> (ld &&) <$> P.readCell rs)
      connect rs newCell (\rd -> (&& rd) <$> P.readCell ls)
      pure (newCell, rvs, xs ++ ys)
    Or left right -> do
      (ls, lvs, xs) <- cellifyConstraint left []
      (rs, rvs, ys) <- cellifyConstraint right []
      newCell <- cell True eqJoin
      connect ls newCell (\ld -> (ld ||) <$> P.readCell rs)
      connect rs newCell (\rd -> (|| rd) <$> P.readCell ls)
      let (pairs, solos) = split lvs rvs
      pairNews <-
        forM pairs $ \(v,lc,rc) -> do
          (varCell, new) <-
            case find ((v ==) . fst) vars of
              Just av -> pure (snd av, [])
              Nothing -> do
                varCell <- cell D.maxDomain domainJoin
                pure (varCell, [(v, varCell)])
          connect varCell lc pure
          connect varCell rc pure
          connect lc varCell (\ld -> D.union ld     <$> P.readCell rc)
          connect rc varCell (\rd -> (`D.union` rd) <$> P.readCell lc)
          pure new
      soloNews <-
        forM solos $ \(v,sc) -> do
          (varCell, new) <-
            case find ((v ==) . fst) vars of
              Just av -> pure (snd av, [])
              Nothing -> do
                varCell <- cell D.maxDomain domainJoin
                pure (varCell, [(v, varCell)])
          connect varCell sc pure
          pure new
      pure (newCell, concat pairNews ++ concat soloNews ++ vars, xs ++ ys)
  where
    binary left right wire = do
      (ls, lcs, xs) <- cellifyExpression left vars
      (rs, rcs, ys) <- cellifyExpression right lcs
      newCell <- cell True eqJoin
      _ <- wire ls rs
      connect ls newCell (pure . not . D.null)
      connect rs newCell (pure . not . D.null)
      pure (newCell, rcs, xs ++ ys)

-- | Tries to find an expression in a cell list. If it is found, it is removed
-- from the list.
extract :: Expression -> [VarCell s] -> Maybe (VarCell s, [VarCell s])
extract _ []     = Nothing
extract a (x:xs) | a == fst x = Just (x, xs)
                 | otherwise  = do (b, rs) <- extract a xs
                                   pure (b, x : rs)

-- | Takes to list of cells and checks if any two entries of the lists stand
-- for the same expression. If so, they land in the first tuple entry of the
-- result (i.e., we pair them). All other "solo entries" of both lists with no
-- counterpart are collected in the second tuple entry.
split :: [VarCell s]
      -> [VarCell s]
      -> ([(Expression, DomainCell s, DomainCell s)], [VarCell s])
split []     right = ([], right)
split (x:xs) right =
  case extract xVar right of
    Just (a, rs) ->
      let (ps, vs) = split xs rs
      in ((xVar, snd x, snd a) : ps, vs)
    Nothing ->
      let (ps, vs) = split xs right
      in (ps, x : vs)
  where
    xVar = fst x