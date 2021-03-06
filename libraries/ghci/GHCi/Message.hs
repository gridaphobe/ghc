{-# LANGUAGE GADTs, DeriveGeneric, StandaloneDeriving,
    GeneralizedNewtypeDeriving, ExistentialQuantification, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}

module GHCi.Message
  ( Message(..), Msg(..)
  , EvalStatus_(..), EvalStatus, EvalResult(..), EvalOpts(..), EvalExpr(..)
  , SerializableException(..)
  , THResult(..), THResultType(..)
  , ResumeContext(..)
  , QState(..)
  , getMessage, putMessage
  , Pipe(..), remoteCall, readPipe, writePipe
  ) where

import GHCi.RemoteTypes
import GHCi.ResolvedBCO
import GHCi.InfoTable (StgInfoTable)
import GHCi.FFI
import GHCi.TH.Binary ()
import GHCi.BreakArray

import GHC.LanguageExtensions
import Control.Concurrent
import Control.Exception
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Dynamic
import Data.IORef
import Data.Map (Map)
import Foreign.C
import GHC.Generics
import GHC.Stack.CCS
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH
import System.Exit
import System.IO
import System.IO.Error

-- -----------------------------------------------------------------------------
-- The RPC protocol between GHC and the interactive server

-- | A @Message a@ is a message that returns a value of type @a@
data Message a where
  -- | Exit the iserv process
  Shutdown :: Message ()

  -- RTS Linker -------------------------------------------

  -- These all invoke the corresponding functions in the RTS Linker API.
  InitLinker :: Message ()
  LookupSymbol :: String -> Message (Maybe (RemotePtr ()))
  LookupClosure :: String -> Message (Maybe HValueRef)
  LoadDLL :: String -> Message (Maybe String)
  LoadArchive :: String -> Message () -- error?
  LoadObj :: String -> Message () -- error?
  UnloadObj :: String -> Message () -- error?
  AddLibrarySearchPath :: String -> Message (RemotePtr ())
  RemoveLibrarySearchPath :: RemotePtr () -> Message Bool
  ResolveObjs :: Message Bool
  FindSystemLibrary :: String -> Message (Maybe String)

  -- Interpreter -------------------------------------------

  -- | Create a set of BCO objects, and return HValueRefs to them
  CreateBCOs :: [ResolvedBCO] -> Message [HValueRef]

  -- | Release 'HValueRef's
  FreeHValueRefs :: [HValueRef] -> Message ()

  -- | Malloc some data and return a 'RemotePtr' to it
  MallocData :: ByteString -> Message (RemotePtr ())

  -- | Calls 'GHCi.FFI.prepareForeignCall'
  PrepFFI :: FFIConv -> [FFIType] -> FFIType -> Message (RemotePtr C_ffi_cif)

  -- | Free data previously created by 'PrepFFI'
  FreeFFI :: RemotePtr C_ffi_cif -> Message ()

  -- | Create an info table for a constructor
  MkConInfoTable
   :: Int     -- ptr words
   -> Int     -- non-ptr words
   -> Int     -- constr tag
   -> [Word8] -- constructor desccription
   -> Message (RemotePtr StgInfoTable)

  -- | Evaluate a statement
  EvalStmt
    :: EvalOpts
    -> EvalExpr HValueRef {- IO [a] -}
    -> Message (EvalStatus [HValueRef]) {- [a] -}

  -- | Resume evaluation of a statement after a breakpoint
  ResumeStmt
   :: EvalOpts
   -> RemoteRef (ResumeContext [HValueRef])
   -> Message (EvalStatus [HValueRef])

  -- | Abandon evaluation of a statement after a breakpoint
  AbandonStmt
   :: RemoteRef (ResumeContext [HValueRef])
   -> Message ()

  -- | Evaluate something of type @IO String@
  EvalString
    :: HValueRef {- IO String -}
    -> Message (EvalResult String)

  -- | Evaluate something of type @String -> IO String@
  EvalStringToString
    :: HValueRef {- String -> IO String -}
    -> String
    -> Message (EvalResult String)

  -- | Evaluate something of type @IO ()@
  EvalIO
   :: HValueRef {- IO a -}
   -> Message (EvalResult ())

  -- | Create a CostCentre
  MkCostCentre
   :: RemotePtr CChar    -- module, RemotePtr so it can be shared
   -> String       -- name
   -> String       -- SrcSpan
   -> Message (RemotePtr CostCentre)

  -- | Show a 'CostCentreStack' as a @[String]@
  CostCentreStackInfo
   :: RemotePtr CostCentreStack
   -> Message [String]

  -- | Create a new array of breakpoint flags
  NewBreakArray
   :: Int                               -- size
   -> Message (RemoteRef BreakArray)

  -- | Enable a breakpoint
  EnableBreakpoint
   :: RemoteRef BreakArray
   -> Int                               -- index
   -> Bool                              -- on or off
   -> Message ()

  -- | Query the status of a breakpoint (True <=> enabled)
  BreakpointStatus
   :: RemoteRef BreakArray
   -> Int                               -- index
   -> Message Bool                      -- True <=> enabled

  -- | Get a reference to a free variable at a breakpoint
  GetBreakpointVar
   :: HValueRef                         -- the AP_STACK from EvalBreak
   -> Int
   -> Message (Maybe HValueRef)

  -- Template Haskell -------------------------------------------

  -- | Start a new TH module, return a state token that should be
  StartTH :: Message (RemoteRef (IORef QState))

  -- | Run TH module finalizers, and free the HValueRef
  FinishTH :: RemoteRef (IORef QState) -> Message ()

  -- | Evaluate a TH computation.
  --
  -- Returns a ByteString, because we have to force the result
  -- before returning it to ensure there are no errors lurking
  -- in it.  The TH types don't have NFData instances, and even if
  -- they did, we have to serialize the value anyway, so we might
  -- as well serialize it to force it.
  RunTH
   :: RemoteRef (IORef QState)
   -> HValueRef {- e.g. TH.Q TH.Exp -}
   -> THResultType
   -> Maybe TH.Loc
   -> Message ByteString {- e.g. TH.Exp -}

  -- Template Haskell Quasi monad operations
  NewName :: String -> Message (THResult TH.Name)
  Report :: Bool -> String -> Message (THResult ())
  LookupName :: Bool -> String -> Message (THResult (Maybe TH.Name))
  Reify :: TH.Name -> Message (THResult TH.Info)
  ReifyFixity :: TH.Name -> Message (THResult (Maybe TH.Fixity))
  ReifyInstances :: TH.Name -> [TH.Type] -> Message (THResult [TH.Dec])
  ReifyRoles :: TH.Name -> Message (THResult [TH.Role])
  ReifyAnnotations :: TH.AnnLookup -> TypeRep -> Message (THResult [ByteString])
  ReifyModule :: TH.Module -> Message (THResult TH.ModuleInfo)
  ReifyConStrictness :: TH.Name -> Message (THResult [TH.DecidedStrictness])

  AddDependentFile :: FilePath -> Message (THResult ())
  AddTopDecls :: [TH.Dec] -> Message (THResult ())
  IsExtEnabled :: Extension -> Message (THResult Bool)
  ExtsEnabled :: Message (THResult [Extension])

  StartRecover :: Message ()
  EndRecover :: Bool -> Message ()

  -- Template Haskell return values

  -- | RunTH finished successfully; return value follows
  QDone :: Message ()
  -- | RunTH threw an exception
  QException :: String -> Message ()
  -- | RunTH called 'fail'
  QFail :: String -> Message ()

deriving instance Show (Message a)

data EvalOpts = EvalOpts
  { useSandboxThread :: Bool
  , singleStep :: Bool
  , breakOnException :: Bool
  , breakOnError :: Bool
  }
  deriving (Generic, Show)

instance Binary EvalOpts

data ResumeContext a = ResumeContext
  { resumeBreakMVar :: MVar ()
  , resumeStatusMVar :: MVar (EvalStatus a)
  , resumeThreadId :: ThreadId
  }

-- | We can pass simple expressions to EvalStmt, consisting of values
-- and application.  This allows us to wrap the statement to be
-- executed in another function, which is used by GHCi to implement
-- :set args and :set prog.  It might be worthwhile to extend this
-- little language in the future.
data EvalExpr a
  = EvalThis a
  | EvalApp (EvalExpr a) (EvalExpr a)
  deriving (Generic, Show)

instance Binary a => Binary (EvalExpr a)

type EvalStatus a = EvalStatus_ a a

data EvalStatus_ a b
  = EvalComplete Word64 (EvalResult a)
  | EvalBreak Bool
       HValueRef{- AP_STACK -}
       Int {- break index -}
       Int {- uniq of ModuleName -}
       (RemoteRef (ResumeContext b))
       (RemotePtr CostCentreStack) -- Cost centre stack
  deriving (Generic, Show)

instance Binary a => Binary (EvalStatus_ a b)

data EvalResult a
  = EvalException SerializableException
  | EvalSuccess a
  deriving (Generic, Show)

instance Binary a => Binary (EvalResult a)

-- SomeException can't be serialized because it contains dynamic
-- types.  However, we do very limited things with the exceptions that
-- are thrown by interpreted computations:
--
-- * We print them, e.g. "*** Exception: <something>"
-- * UserInterrupt has a special meaning
-- * In ghc -e, exitWith should exit with the appropriate exit code
--
-- So all we need to do is distinguish UserInterrupt and ExitCode, and
-- all other exceptions can be represented by their 'show' string.
--
data SerializableException
  = EUserInterrupt
  | EExitCode ExitCode
  | EOtherException String
  deriving (Generic, Show)

instance Binary ExitCode
instance Binary SerializableException

data THResult a
  = THException String
  | THComplete a
  deriving (Generic, Show)

instance Binary a => Binary (THResult a)

data THResultType = THExp | THPat | THType | THDec | THAnnWrapper
  deriving (Enum, Show, Generic)

instance Binary THResultType

data QState = QState
  { qsMap        :: Map TypeRep Dynamic
       -- ^ persistent data between splices in a module
  , qsFinalizers :: [TH.Q ()]
       -- ^ registered finalizers (in reverse order)
  , qsLocation   :: Maybe TH.Loc
       -- ^ location for current splice, if any
  , qsPipe :: Pipe
       -- ^ pipe to communicate with GHC
  }
instance Show QState where show _ = "<QState>"

data Msg = forall a . (Binary a, Show a) => Msg (Message a)

getMessage :: Get Msg
getMessage = do
    b <- getWord8
    case b of
      0  -> Msg <$> return Shutdown
      1  -> Msg <$> return InitLinker
      2  -> Msg <$> LookupSymbol <$> get
      3  -> Msg <$> LookupClosure <$> get
      4  -> Msg <$> LoadDLL <$> get
      5  -> Msg <$> LoadArchive <$> get
      6  -> Msg <$> LoadObj <$> get
      7  -> Msg <$> UnloadObj <$> get
      8  -> Msg <$> AddLibrarySearchPath <$> get
      9  -> Msg <$> RemoveLibrarySearchPath <$> get
      10 -> Msg <$> return ResolveObjs
      11 -> Msg <$> FindSystemLibrary <$> get
      12 -> Msg <$> CreateBCOs <$> get
      13 -> Msg <$> FreeHValueRefs <$> get
      14 -> Msg <$> MallocData <$> get
      15 -> Msg <$> (PrepFFI <$> get <*> get <*> get)
      16 -> Msg <$> FreeFFI <$> get
      17 -> Msg <$> (MkConInfoTable <$> get <*> get <*> get <*> get)
      18 -> Msg <$> (EvalStmt <$> get <*> get)
      19 -> Msg <$> (ResumeStmt <$> get <*> get)
      20 -> Msg <$> (AbandonStmt <$> get)
      21 -> Msg <$> (EvalString <$> get)
      22 -> Msg <$> (EvalStringToString <$> get <*> get)
      23 -> Msg <$> (EvalIO <$> get)
      24 -> Msg <$> (MkCostCentre <$> get <*> get <*> get)
      25 -> Msg <$> (CostCentreStackInfo <$> get)
      26 -> Msg <$> (NewBreakArray <$> get)
      27 -> Msg <$> (EnableBreakpoint <$> get <*> get <*> get)
      28 -> Msg <$> (BreakpointStatus <$> get <*> get)
      29 -> Msg <$> (GetBreakpointVar <$> get <*> get)
      30 -> Msg <$> return StartTH
      31 -> Msg <$> FinishTH <$> get
      32 -> Msg <$> (RunTH <$> get <*> get <*> get <*> get)
      33 -> Msg <$> NewName <$> get
      34 -> Msg <$> (Report <$> get <*> get)
      35 -> Msg <$> (LookupName <$> get <*> get)
      36 -> Msg <$> Reify <$> get
      37 -> Msg <$> ReifyFixity <$> get
      38 -> Msg <$> (ReifyInstances <$> get <*> get)
      39 -> Msg <$> ReifyRoles <$> get
      40 -> Msg <$> (ReifyAnnotations <$> get <*> get)
      41 -> Msg <$> ReifyModule <$> get
      42 -> Msg <$> ReifyConStrictness <$> get
      43 -> Msg <$> AddDependentFile <$> get
      44 -> Msg <$> AddTopDecls <$> get
      45 -> Msg <$> (IsExtEnabled <$> get)
      46 -> Msg <$> return ExtsEnabled
      47 -> Msg <$> return StartRecover
      48 -> Msg <$> EndRecover <$> get
      49 -> Msg <$> return QDone
      50 -> Msg <$> QException <$> get
      _  -> Msg <$> QFail <$> get

putMessage :: Message a -> Put
putMessage m = case m of
  Shutdown                    -> putWord8 0
  InitLinker                  -> putWord8 1
  LookupSymbol str            -> putWord8 2  >> put str
  LookupClosure str           -> putWord8 3  >> put str
  LoadDLL str                 -> putWord8 4  >> put str
  LoadArchive str             -> putWord8 5  >> put str
  LoadObj str                 -> putWord8 6  >> put str
  UnloadObj str               -> putWord8 7  >> put str
  AddLibrarySearchPath str    -> putWord8 8  >> put str
  RemoveLibrarySearchPath ptr -> putWord8 9  >> put ptr
  ResolveObjs                 -> putWord8 10
  FindSystemLibrary str       -> putWord8 11 >> put str
  CreateBCOs bco              -> putWord8 12 >> put bco
  FreeHValueRefs val          -> putWord8 13 >> put val
  MallocData bs               -> putWord8 14 >> put bs
  PrepFFI conv args res       -> putWord8 15 >> put conv >> put args >> put res
  FreeFFI p                   -> putWord8 16 >> put p
  MkConInfoTable p n t d      -> putWord8 17 >> put p >> put n >> put t >> put d
  EvalStmt opts val           -> putWord8 18 >> put opts >> put val
  ResumeStmt opts val         -> putWord8 19 >> put opts >> put val
  AbandonStmt val             -> putWord8 20 >> put val
  EvalString val              -> putWord8 21 >> put val
  EvalStringToString str val  -> putWord8 22 >> put str >> put val
  EvalIO val                  -> putWord8 23 >> put val
  MkCostCentre mod name src   -> putWord8 24 >> put mod >> put name >> put src
  CostCentreStackInfo ptr     -> putWord8 25 >> put ptr
  NewBreakArray sz            -> putWord8 26 >> put sz
  EnableBreakpoint arr ix b   -> putWord8 27 >> put arr >> put ix >> put b
  BreakpointStatus arr ix     -> putWord8 28 >> put arr >> put ix
  GetBreakpointVar a b        -> putWord8 29 >> put a >> put b
  StartTH                     -> putWord8 30
  FinishTH val                -> putWord8 31 >> put val
  RunTH st q loc ty           -> putWord8 32 >> put st >> put q >> put loc >> put ty
  NewName a                   -> putWord8 33 >> put a
  Report a b                  -> putWord8 34 >> put a >> put b
  LookupName a b              -> putWord8 35 >> put a >> put b
  Reify a                     -> putWord8 36 >> put a
  ReifyFixity a               -> putWord8 37 >> put a
  ReifyInstances a b          -> putWord8 38 >> put a >> put b
  ReifyRoles a                -> putWord8 39 >> put a
  ReifyAnnotations a b        -> putWord8 40 >> put a >> put b
  ReifyModule a               -> putWord8 41 >> put a
  ReifyConStrictness a        -> putWord8 42 >> put a
  AddDependentFile a          -> putWord8 43 >> put a
  AddTopDecls a               -> putWord8 44 >> put a
  IsExtEnabled a              -> putWord8 45 >> put a
  ExtsEnabled                 -> putWord8 46
  StartRecover                -> putWord8 47
  EndRecover a                -> putWord8 48 >> put a
  QDone                       -> putWord8 49
  QException a                -> putWord8 50 >> put a
  QFail a                     -> putWord8 51  >> put a

-- -----------------------------------------------------------------------------
-- Reading/writing messages

data Pipe = Pipe
  { pipeRead :: Handle
  , pipeWrite ::  Handle
  , pipeLeftovers :: IORef (Maybe ByteString)
  }

remoteCall :: Binary a => Pipe -> Message a -> IO a
remoteCall pipe msg = do
  writePipe pipe (putMessage msg)
  readPipe pipe get

writePipe :: Pipe -> Put -> IO ()
writePipe Pipe{..} put
  | LB.null bs = return ()
  | otherwise  = do
    LB.hPut pipeWrite bs
    hFlush pipeWrite
 where
  bs = runPut put

readPipe :: Pipe -> Get a -> IO a
readPipe Pipe{..} get = do
  leftovers <- readIORef pipeLeftovers
  m <- getBin pipeRead get leftovers
  case m of
    Nothing -> throw $
      mkIOError eofErrorType "GHCi.Message.remoteCall" (Just pipeRead) Nothing
    Just (result, new_leftovers) -> do
      writeIORef pipeLeftovers new_leftovers
      return result

getBin
  :: Handle -> Get a -> Maybe ByteString
  -> IO (Maybe (a, Maybe ByteString))

getBin h get leftover = go leftover (runGetIncremental get)
 where
   go Nothing (Done leftover _ msg) =
     return (Just (msg, if B.null leftover then Nothing else Just leftover))
   go _ Done{} = throwIO (ErrorCall "getBin: Done with leftovers")
   go (Just leftover) (Partial fun) = do
     go Nothing (fun (Just leftover))
   go Nothing (Partial fun) = do
     -- putStrLn "before hGetSome"
     b <- B.hGetSome h (32*1024)
     -- printf "hGetSome: %d\n" (B.length b)
     if B.null b
        then return Nothing
        else go Nothing (fun (Just b))
   go _lft (Fail _rest _off str) =
     throwIO (ErrorCall ("getBin: " ++ str))
