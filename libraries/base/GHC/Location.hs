{-# LANGUAGE RecordWildCards #-}
module GHC.Location
  ( CallStack
  , getCallStack

  , SrcLoc
  , srcLocPackage
  , srcLocModule
  , srcLocFile
  , srcLocStartLine
  , srcLocStartCol
  , srcLocEndLine
  , srcLocEndCol

  -- * Pretty printing
  , showCallStack
  , showSrcLoc
  ) where

--------------------------------------------------------------------------------
-- Source Locations
--------------------------------------------------------------------------------

-- | A CallStack is comprised of at least one 'SrcLoc'.
data CallStack = CallStack { getCallStack :: [SrcLoc] }
  deriving (Show, Eq)

showCallStack :: CallStack -> String
showCallStack (CallStack (root:rest))
  = unlines (showSrcLoc root : map (indent . showSrcLoc) rest)
  where
  indent l = "  " ++ l
showCallStack _ = error "CallStack cannot be empty!"

-- | A single location in the source code.
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
