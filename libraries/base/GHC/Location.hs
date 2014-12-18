{-# LANGUAGE RecordWildCards #-}
module GHC.Location
  ( Location
  , getLocation

  , SrcLoc
  , srcLocPackage
  , srcLocModule
  , srcLocFile
  , srcLocStartLine
  , srcLocStartCol
  , srcLocEndLine
  , srcLocEndCol

  -- * Pretty printing
  , showLocation
  , showSrcLoc
  ) where

--------------------------------------------------------------------------------
-- Source Locations
--------------------------------------------------------------------------------

-- | A location in the source program is comprised of multiple 'SrcLoc's,
-- forming an explicit call-stack.
data Location = Location { getLocation :: [SrcLoc] }
  deriving (Show, Eq)

showLocation :: Location -> String
showLocation (Location (root:rest))
  = unlines (showSrcLoc root : map (indent . showSrcLoc) rest)
  where
  indent l = "  " ++ l
showLocation _ = error "Location cannot be empty!"

-- | A single source span.
data SrcLoc = SrcLoc
  { srcLocPackage   :: String
  , srcLocModule    :: String
  , srcLocFile      :: String
  , srcLocStartLine :: Int
  , srcLocStartCol  :: Int
  , srcLocEndLine   :: Int
  , srcLocEndCol    :: Int
  } deriving (Show, Eq)

showSrcLoc :: SrcLoc -> String
showSrcLoc SrcLoc {..}
  = concat [ srcLocFile, ":"
           , show srcLocStartLine, ":"
           , show srcLocStartCol, " in "
           , srcLocPackage, ":", srcLocModule
           ]
