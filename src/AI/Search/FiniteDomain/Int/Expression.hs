module AI.Search.FiniteDomain.Int.Expression
  ( Expression
  , cellifyExpression
  , int
  , sum
  , var
  ) where

-- base
import Control.Monad.ST ( ST )
import Data.List        ( find )
import Prelude hiding   ( sum )

-- domain
import Numeric.Domain as D ( Domain, div, inverseAbs, inverseSignum, singleton
                           , maxDomain )

-- propeller
import Data.Propagator.Cell     ( Cell, cell )
import Data.Propagator.Num as P ( absWith, plus, minus, timesWith, negate
                                , signumWith )

import AI.Search.FiniteDomain.Int.Cell ( domainJoin )

-- | Expressions are the build blocks of constraints. Note that 'Expression' is
-- an instance of 'Num', so arithmetic combinations of expressions are possible.
data Expression
  = Int    Int
  | Var    Int
  | Plus   Expression Expression
  | Minus  Expression Expression
  | Times  Expression Expression
  | Negate Expression
  | Abs    Expression
  | Signum Expression
  deriving (Eq, Ord, Show)

instance Num Expression where
  (+)         = Plus
  (-)         = Minus
  (*)         = Times
  negate      = Negate
  abs         = Abs
  signum      = Signum
  fromInteger = Int . fromInteger

type DomainCell s = Cell s (Domain Int)
type VarCell s    = (Expression, DomainCell s)

-- | Converts an expression to a propagator cell.
-- The result consists of the new cell that represents the expression, a list
-- of currently declared variables, and a list of all cells that were created
-- for this expression.
cellifyExpression
  :: Expression
  -> [VarCell s]
  -> ST s (DomainCell s, [VarCell s], [DomainCell s])
cellifyExpression expr vars =
  case expr of
    Int i            -> atomic (singleton i)
    Var _            -> atomic maxDomain
    Plus left right  -> binary left right plus
    Minus left right -> binary left right minus
    Times left right -> binary left right (timesWith D.div)
    Negate arg       -> unary arg P.negate
    Abs arg          -> unary arg (absWith inverseAbs)
    Signum arg       -> unary arg (signumWith inverseSignum)
  where
    atomic initValue =
      case find ((expr ==) . fst) vars of
        Just ce -> pure (snd ce, vars, [])
        Nothing -> do newCell <- cell initValue domainJoin
                      pure (newCell, (expr, newCell) : vars, [newCell])
    unary arg wire = do
      (es, rvs, xs) <- cellifyExpression arg vars
      newCell <- cell maxDomain domainJoin
      _ <- wire es newCell
      pure (newCell, rvs, xs)
    binary left right wire = do
      (ls, nvs, xs) <- cellifyExpression left vars
      (rs, rvs, ys) <- cellifyExpression right nvs
      newCell <- cell maxDomain domainJoin
      _ <- wire ls rs newCell
      pure (newCell, rvs, xs ++ ys)

-- | Converts an integer into an expression that can be used in constraint
-- formulations.
int :: Int -> Expression
int = Int

-- | Converts a variable ID into an expression that can be used in constraint
-- formulations.
var :: Int -> Expression
var = Var

-- | Sums up a list of expressions.
sum :: [Expression] -> Expression
sum []     = int 0
sum (e:es) = e + sum es