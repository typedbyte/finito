-- This module exports some useful join functions for propagator cells.
-- A join function describes how an old cell value is combined with a new one.
-- See the package @propeller@ for details on the propagator implementation.
module AI.Search.FiniteDomain.Int.Cell
  ( domainJoin
  , eqJoin
  , mustHoldJoin
  ) where

-- domain
import Numeric.Domain ( Domain, intersect, isSubsetOf )

-- propeller
import Data.Propagator.Change ( Change(..) )

-- | Compares an old and a new domain of integer values to see if the new
-- domain is a subset of the old domain (i.e., the cell has changed).
--
-- Never returns 'Incompatible'.
domainJoin :: Domain Int -> Domain Int -> Change (Domain Int)
domainJoin old new =
  case intersect old new of
    reduced | reduced `isSubsetOf` old -> Changed reduced
            | otherwise                -> Unchanged

-- | Compares an old and a new value to see if there was a change.
--
-- Never returns 'Incompatible'.
eqJoin :: Eq a => a -> a -> Change a
eqJoin old new
  | old == new = Unchanged
  | otherwise  = Changed new

-- | Checks if a new cell value is ...
--
-- * 'True', then 'Unchanged' is returned, or
-- * 'False', then 'Incompatible' is returned.
--
-- This can be used to represent conditions which must always hold
-- (i.e., the cell value must always be 'True').
--
-- The old value of the cell is ignored. Never returns 'Changed'.
mustHoldJoin :: a -> Bool -> Change b
mustHoldJoin _ True  = Unchanged
mustHoldJoin _ False = Incompatible