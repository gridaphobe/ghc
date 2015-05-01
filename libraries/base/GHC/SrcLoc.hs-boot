{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.SrcLoc
  ( SrcLoc
  , srcLocPackage
  , srcLocModule
  , srcLocFile
  , srcLocStartLine
  , srcLocStartCol
  , srcLocEndLine
  , srcLocEndCol
  ) where

import GHC.Types( Char, Int )

-- | A single location in the source code.
data SrcLoc = SrcLoc
  { srcLocPackage   :: [Char]
  , srcLocModule    :: [Char]
  , srcLocFile      :: [Char]
  , srcLocStartLine :: Int
  , srcLocStartCol  :: Int
  , srcLocEndLine   :: Int
  , srcLocEndCol    :: Int
  }
