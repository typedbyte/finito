-----------------------------------------------------------------------------
-- |
-- Module      :  AI.Search.FiniteDomain.Int
-- Copyright   :  (c) Michael Szvetits, 2020
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- This module exports the types and functions needed to define constraints
-- and run the constraint solver.
-----------------------------------------------------------------------------
module AI.Search.FiniteDomain.Int
  ( -- * Core Monad
    FD
    -- * Building Expressions
  , Expression
  , int
  , newVar
  , initNewVar
  , sum
    -- * Building Constraints
  , Constraint
  , (#=)
  , (#/=)
  , (#<)
  , (#<=)
  , (#>)
  , (#>=)
  , (/\)
  , (\/)
  , not'
  , between
  , allDifferent
  -- * Running the Solver
  , Labeling(..)
  , labeling
  , runFD
  ) where

-- base
import Prelude hiding ( sum )

import AI.Search.FiniteDomain.Int.Constraint
import AI.Search.FiniteDomain.Int.Expression ( Expression, int, sum )