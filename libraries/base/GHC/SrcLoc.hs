{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}

-- | @since 4.8.2.0
module GHC.SrcLoc
  ( SrcLoc
  , srcLocPackage
  , srcLocModule
  , srcLocFile
  , srcLocStartLine
  , srcLocStartCol
  , srcLocEndLine
  , srcLocEndCol

  -- * Pretty printing
  , showSrcLoc
  ) where

import GHC.Base
import GHC.Show

-- | Pretty print 'SrcLoc'
--
-- @since 4.8.2.0
showSrcLoc :: SrcLoc -> [Char]
showSrcLoc SrcLoc {..}
  = foldr (++) ""
      [ srcLocFile, ":"
      , show srcLocStartLine, ":"
      , show srcLocStartCol, " in "
      , srcLocPackage, ":", srcLocModule
      ]

