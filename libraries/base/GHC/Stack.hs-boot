-- This SOURCE-imported hs-boot module cuts a dependency loop:
--
--          GHC.Stack
-- imports  GHC.Base
-- imports  GHC.Err
-- imports  {-# SOURCE #-} GHC.Stack

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Stack ( CallStack, showCallStack ) where

import {-# SOURCE #-} GHC.SrcLoc ( SrcLoc )
import GHC.Types ( Char )

data CallStack = CallStack { getCallStack :: [([Char], SrcLoc)] }

showCallStack :: CallStack -> [Char]
