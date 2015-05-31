{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Err
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- The "GHC.Err" module defines the code for the wired-in error functions,
-- which have a special type in the compiler (with \"open tyvars\").
--
-- We cannot define these functions in a module where they might be used
-- (e.g., "GHC.Base"), because the magical wired-in type will get confused
-- with what the typechecker figures out.
--
-----------------------------------------------------------------------------

module GHC.Err( error ) where
import GHC.CString ()
import GHC.Types
import GHC.Prim
import GHC.Integer ()   -- Make sure Integer is compiled first
                        -- because GHC depends on it in a wired-in way
                        -- so the build system doesn't see the dependency
import {-# SOURCE #-} GHC.Exception( errorCallException )

-- | 'error' stops execution and displays an error message.
error :: [Char] -> CallStack -> a
error s stk = raise# (errorCallException s stk)
