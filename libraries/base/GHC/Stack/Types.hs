{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Stack.Types
-- Copyright   :  (c) The University of Glasgow 2015
-- License     :  see libraries/ghc-prim/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- type definitions for call-stacks via implicit parameters.
-- Use GHC.Exts from the base package instead of importing this
-- module directly.
--
-----------------------------------------------------------------------------

module GHC.Stack.Types (
    -- * Implicit parameter call stacks
    CallStack, getCallStack, pushCallStack,
    -- * Source locations
    SrcLoc(..)
  ) where

{-
Ideally these would live in GHC.Stack but sadly they can't due to this
import cycle,

    Module imports form a cycle:
           module ‘Data.Maybe’ (libraries/base/Data/Maybe.hs)
          imports ‘GHC.Base’ (libraries/base/GHC/Base.hs)
    which imports ‘GHC.Err’ (libraries/base/GHC/Err.hs)
    which imports ‘GHC.Stack’ (libraries/base/dist-install/build/GHC/Stack.hs)
    which imports ‘GHC.Foreign’ (libraries/base/GHC/Foreign.hs)
    which imports ‘Data.Maybe’ (libraries/base/Data/Maybe.hs)
-}

import GHC.Types

-- Make implicit dependency known to build system
import GHC.Tuple ()
import GHC.Integer ()

----------------------------------------------------------------------
-- Explicit call-stacks built via ImplicitParams
----------------------------------------------------------------------

-- | Implicit @CallStack@s are an alternate method of obtaining the call stack
-- at a given point in the program.
--
-- GHC has two built-in rules for solving implicit-parameters of type
-- @CallStack@.
--
-- 1. If the @CallStack@ occurs in a function call, it appends the
--    source location of the call to the @CallStack@ in the environment.
-- 2. @CallStack@s that cannot be solved normally (i.e. unbound
--    occurrences) are defaulted to the empty @CallStack@.
--
-- Otherwise implicit @CallStack@s behave just like ordinary implicit
-- parameters. For example:
--
-- @
-- myerror :: (?callStack :: CallStack) => String -> a
-- myerror msg = error (msg ++ "\n" ++ prettyCallStack ?callStack)
-- @
--
-- ghci> myerror "die"
-- *** Exception: die
-- CallStack (from ImplicitParams):
--   myerror, called at <interactive>:2:1 in interactive:Ghci1
--
-- @CallStack@s do not interact with the RTS and do not require compilation with
-- @-prof@. On the other hand, as they are built up explicitly using
-- implicit-parameters, they will generally not contain as much information as
-- the simulated call-stacks maintained by the RTS.
--
-- A @CallStack@ is a @[(String, SrcLoc)]@. The @String@ is the name of
-- function that was called, the 'SrcLoc' is the call-site. The list is
-- ordered with the most recently called function at the head.
--
-- @since 4.8.1.0
data CallStack = CallStack { getCallStack :: [([Char], SrcLoc)] }
  -- See Note [Overview of implicit CallStacks]


-- | Push a call-site onto the stack.
pushCallStack :: ([Char], SrcLoc) -> CallStack -> CallStack
pushCallStack callSite (CallStack stk)
  = CallStack (callSite : stk)


-- | A single location in the source code.
--
-- @since 4.8.1.0
data SrcLoc = SrcLoc
  { srcLocPackage   :: [Char]
  , srcLocModule    :: [Char]
  , srcLocFile      :: [Char]
  , srcLocStartLine :: Int
  , srcLocStartCol  :: Int
  , srcLocEndLine   :: Int
  , srcLocEndCol    :: Int
  }
