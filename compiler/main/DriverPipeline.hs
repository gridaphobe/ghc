{-# LANGUAGE CPP, NamedFieldPuns, NondecreasingIndentation #-}
{-# OPTIONS_GHC -fno-cse #-}
-- -fno-cse is needed for GLOBAL_VAR's to behave properly

-----------------------------------------------------------------------------
--
-- GHC Driver
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module DriverPipeline (
        -- Run a series of compilation steps in a pipeline, for a
        -- collection of source files.
   oneShot, compileFile,

        -- Interfaces for the batch-mode driver
   linkBinary,

        -- Interfaces for the compilation manager (interpreted/batch-mode)
   preprocess,
   compileOne, compileOne',
   link,

        -- Exports for hooks to override runPhase and link
   PhasePlus(..), CompPipeline(..), PipeEnv(..), PipeState(..),
   phaseOutputFilename, getOutputFilename, getPipeState, getPipeEnv,
   hscPostBackendPhase, getLocation, setModLocation, setDynFlags,
   runPhase, exeFileName,
   mkExtraObjToLinkIntoBinary, mkNoteObjsToLinkIntoBinary,
   maybeCreateManifest,
   linkingNeeded, checkLinkInfo, writeInterfaceOnlyMode
  ) where

#include "HsVersions.h"

import PipelineMonad
import Packages
import HeaderInfo
import DriverPhases
import SysTools
import Elf
import HscMain
import Finder
import HscTypes hiding ( Hsc )
import Outputable
import Module
import ErrUtils
import DynFlags
import Config
import Panic
import Util
import StringBuffer     ( hGetStringBuffer )
import BasicTypes       ( SuccessFlag(..) )
import Maybes           ( expectJust )
import SrcLoc
import LlvmCodeGen      ( llvmFixupAsm )
import MonadUtils
import Platform
import TcRnTypes
import Hooks
import qualified GHC.LanguageExtensions as LangExt

import Exception
import System.Directory
import System.FilePath
import System.IO
import Control.Monad
import Data.List        ( isSuffixOf )
import Data.Maybe
import Data.Version

-- ---------------------------------------------------------------------------
-- Pre-process

-- | Just preprocess a file, put the result in a temp. file (used by the
-- compilation manager during the summary phase).
--
-- We return the augmented DynFlags, because they contain the result
-- of slurping in the OPTIONS pragmas

preprocess :: HscEnv
           -> (FilePath, Maybe Phase) -- ^ filename and starting phase
           -> IO (DynFlags, FilePath)
preprocess hsc_env (filename, mb_phase) =
  ASSERT2(isJust mb_phase || isHaskellSrcFilename filename, text filename)
  runPipeline anyHsc hsc_env (filename, fmap RealPhase mb_phase)
        Nothing Temporary Nothing{-no ModLocation-} Nothing{-no stub-}

-- ---------------------------------------------------------------------------

-- | Compile
--
-- Compile a single module, under the control of the compilation manager.
--
-- This is the interface between the compilation manager and the
-- compiler proper (hsc), where we deal with tedious details like
-- reading the OPTIONS pragma from the source file, converting the
-- C or assembly that GHC produces into an object file, and compiling
-- FFI stub files.
--
-- NB.  No old interface can also mean that the source has changed.

compileOne :: HscEnv
           -> ModSummary      -- ^ summary for module being compiled
           -> Int             -- ^ module N ...
           -> Int             -- ^ ... of M
           -> Maybe ModIface  -- ^ old interface, if we have one
           -> Maybe Linkable  -- ^ old linkable, if we have one
           -> SourceModified
           -> IO HomeModInfo   -- ^ the complete HomeModInfo, if successful

compileOne = compileOne' Nothing (Just batchMsg)

compileOne' :: Maybe TcGblEnv
            -> Maybe Messager
            -> HscEnv
            -> ModSummary      -- ^ summary for module being compiled
            -> Int             -- ^ module N ...
            -> Int             -- ^ ... of M
            -> Maybe ModIface  -- ^ old interface, if we have one
            -> Maybe Linkable  -- ^ old linkable, if we have one
            -> SourceModified
            -> IO HomeModInfo   -- ^ the complete HomeModInfo, if successful

compileOne' m_tc_result mHscMessage
            hsc_env0 summary mod_index nmods mb_old_iface maybe_old_linkable
            source_modified0
 = do

   debugTraceMsg dflags1 2 (text "compile: input file" <+> text input_fnpp)

   (status, hmi0) <- hscIncrementalCompile
                        always_do_basic_recompilation_check
                        m_tc_result mHscMessage
                        hsc_env summary source_modified mb_old_iface (mod_index, nmods)

   let flags = hsc_dflags hsc_env0
     in do unless (gopt Opt_KeepHiFiles flags) $
               addFilesToClean flags [ml_hi_file $ ms_location summary]
           unless (gopt Opt_KeepOFiles flags) $
               addFilesToClean flags [ml_obj_file $ ms_location summary]

   case (status, hsc_lang) of
        (HscUpToDate, _) ->
            -- TODO recomp014 triggers this assert. What's going on?!
            -- ASSERT( isJust maybe_old_linkable || isNoLink (ghcLink dflags) )
            return hmi0 { hm_linkable = maybe_old_linkable }
        (HscNotGeneratingCode, HscNothing) ->
            let mb_linkable = if isHsBootOrSig src_flavour
                                then Nothing
                                -- TODO: Questionable.
                                else Just (LM (ms_hs_date summary) this_mod [])
            in return hmi0 { hm_linkable = mb_linkable }
        (HscNotGeneratingCode, _) -> panic "compileOne HscNotGeneratingCode"
        (_, HscNothing) -> panic "compileOne HscNothing"
        (HscUpdateBoot, HscInterpreted) -> do
            return hmi0
        (HscUpdateBoot, _) -> do
            touchObjectFile dflags object_filename
            return hmi0
        (HscUpdateSig, HscInterpreted) ->
            let linkable = LM (ms_hs_date summary) this_mod []
            in return hmi0 { hm_linkable = Just linkable }
        (HscUpdateSig, _) -> do
            output_fn <- getOutputFilename next_phase
                            Temporary basename dflags next_phase (Just location)

            -- #10660: Use the pipeline instead of calling
            -- compileEmptyStub directly, so -dynamic-too gets
            -- handled properly
            _ <- runPipeline StopLn hsc_env
                              (output_fn,
                               Just (HscOut src_flavour
                                            mod_name HscUpdateSig))
                              (Just basename)
                              Persistent
                              (Just location)
                              Nothing
            o_time <- getModificationUTCTime object_filename
            let linkable = LM o_time this_mod [DotO object_filename]
            return hmi0 { hm_linkable = Just linkable }
        (HscRecomp cgguts summary, HscInterpreted) -> do
            (hasStub, comp_bc, spt_entries) <-
                hscInteractive hsc_env cgguts summary

            stub_o <- case hasStub of
                      Nothing -> return []
                      Just stub_c -> do
                          stub_o <- compileStub hsc_env stub_c
                          return [DotO stub_o]

            let hs_unlinked = [BCOs comp_bc spt_entries]
                unlinked_time = ms_hs_date summary
              -- Why do we use the timestamp of the source file here,
              -- rather than the current time?  This works better in
              -- the case where the local clock is out of sync
              -- with the filesystem's clock.  It's just as accurate:
              -- if the source is modified, then the linkable will
              -- be out of date.
            let linkable = LM unlinked_time (ms_mod summary)
                           (hs_unlinked ++ stub_o)
            return hmi0 { hm_linkable = Just linkable }
        (HscRecomp cgguts summary, _) -> do
            output_fn <- getOutputFilename next_phase
                            Temporary basename dflags next_phase (Just location)
            -- We're in --make mode: finish the compilation pipeline.
            _ <- runPipeline StopLn hsc_env
                              (output_fn,
                               Just (HscOut src_flavour mod_name (HscRecomp cgguts summary)))
                              (Just basename)
                              Persistent
                              (Just location)
                              Nothing
                  -- The object filename comes from the ModLocation
            o_time <- getModificationUTCTime object_filename
            let linkable = LM o_time this_mod [DotO object_filename]
            return hmi0 { hm_linkable = Just linkable }

 where dflags0     = ms_hspp_opts summary

       this_mod    = ms_mod summary
       location    = ms_location summary
       input_fn    = expectJust "compile:hs" (ml_hs_file location)
       input_fnpp  = ms_hspp_file summary
       mod_graph   = hsc_mod_graph hsc_env0
       needsTH     = any (xopt LangExt.TemplateHaskell . ms_hspp_opts) mod_graph
       needsQQ     = any (xopt LangExt.QuasiQuotes     . ms_hspp_opts) mod_graph
       needsLinker = needsTH || needsQQ
       isDynWay    = any (== WayDyn) (ways dflags0)
       isProfWay   = any (== WayProf) (ways dflags0)
       internalInterpreter = not (gopt Opt_ExternalInterpreter dflags0)

       src_flavour = ms_hsc_src summary
       mod_name = ms_mod_name summary
       next_phase = hscPostBackendPhase dflags src_flavour hsc_lang
       object_filename = ml_obj_file location

       -- #8180 - when using TemplateHaskell, switch on -dynamic-too so
       -- the linker can correctly load the object files.  This isn't necessary
       -- when using -fexternal-interpreter.
       dflags1 = if needsLinker && dynamicGhc && internalInterpreter &&
                    not isDynWay && not isProfWay
                  then gopt_set dflags0 Opt_BuildDynamicToo
                  else dflags0

       basename = dropExtension input_fn

       -- We add the directory in which the .hs files resides) to the import
       -- path.  This is needed when we try to compile the .hc file later, if it
       -- imports a _stub.h file that we created here.
       current_dir = takeDirectory basename
       old_paths   = includePaths dflags1
       dflags      = dflags1 { includePaths = current_dir : old_paths }
       hsc_env     = hsc_env0 {hsc_dflags = dflags}

       -- Figure out what lang we're generating
       hsc_lang = hscTarget dflags

       -- -fforce-recomp should also work with --make
       force_recomp = gopt Opt_ForceRecomp dflags
       source_modified
         | force_recomp = SourceModified
         | otherwise = source_modified0

       always_do_basic_recompilation_check = case hsc_lang of
                                             HscInterpreted -> True
                                             _ -> False

-----------------------------------------------------------------------------
-- stub .h and .c files (for foreign export support)

-- The _stub.c file is derived from the haskell source file, possibly taking
-- into account the -stubdir option.
--
-- The object file created by compiling the _stub.c file is put into a
-- temporary file, which will be later combined with the main .o file
-- (see the MergeStubs phase).

compileStub :: HscEnv -> FilePath -> IO FilePath
compileStub hsc_env stub_c = do
        (_, stub_o) <- runPipeline StopLn hsc_env (stub_c,Nothing)  Nothing
                                   Temporary Nothing{-no ModLocation-} Nothing

        return stub_o

compileEmptyStub :: DynFlags -> HscEnv -> FilePath -> ModLocation -> ModuleName -> IO ()
compileEmptyStub dflags hsc_env basename location mod_name = do
  -- To maintain the invariant that every Haskell file
  -- compiles to object code, we make an empty (but
  -- valid) stub object file for signatures.  However,
  -- we make sure this object file has a unique symbol,
  -- so that ranlib on OS X doesn't complain, see
  -- http://ghc.haskell.org/trac/ghc/ticket/12673
  -- and https://github.com/haskell/cabal/issues/2257
  empty_stub <- newTempName dflags "c"
  let src = text "int" <+> ppr (mkModule (thisPackage dflags) mod_name) <+> text "= 0;"
  writeFile empty_stub (showSDoc dflags (pprCode CStyle src))
  _ <- runPipeline StopLn hsc_env
                  (empty_stub, Nothing)
                  (Just basename)
                  Persistent
                  (Just location)
                  Nothing
  return ()

-- ---------------------------------------------------------------------------
-- Link

link :: GhcLink                 -- interactive or batch
     -> DynFlags                -- dynamic flags
     -> Bool                    -- attempt linking in batch mode?
     -> HomePackageTable        -- what to link
     -> IO SuccessFlag

-- For the moment, in the batch linker, we don't bother to tell doLink
-- which packages to link -- it just tries all that are available.
-- batch_attempt_linking should only be *looked at* in batch mode.  It
-- should only be True if the upsweep was successful and someone
-- exports main, i.e., we have good reason to believe that linking
-- will succeed.

link ghcLink dflags
  = lookupHook linkHook l dflags ghcLink dflags
  where
    l LinkInMemory _ _ _
      = if cGhcWithInterpreter == "YES"
        then -- Not Linking...(demand linker will do the job)
             return Succeeded
        else panicBadLink LinkInMemory

    l NoLink _ _ _
      = return Succeeded

    l LinkBinary dflags batch_attempt_linking hpt
      = link' dflags batch_attempt_linking hpt

    l LinkStaticLib dflags batch_attempt_linking hpt
      = link' dflags batch_attempt_linking hpt

    l LinkDynLib dflags batch_attempt_linking hpt
      = link' dflags batch_attempt_linking hpt

panicBadLink :: GhcLink -> a
panicBadLink other = panic ("link: GHC not built to link this way: " ++
                            show other)

link' :: DynFlags                -- dynamic flags
      -> Bool                    -- attempt linking in batch mode?
      -> HomePackageTable        -- what to link
      -> IO SuccessFlag

link' dflags batch_attempt_linking hpt
   | batch_attempt_linking
   = do
        let
            staticLink = case ghcLink dflags of
                          LinkStaticLib -> True
                          _ -> platformBinariesAreStaticLibs (targetPlatform dflags)

            home_mod_infos = eltsHpt hpt

            -- the packages we depend on
            pkg_deps  = concatMap (map fst . dep_pkgs . mi_deps . hm_iface) home_mod_infos

            -- the linkables to link
            linkables = map (expectJust "link".hm_linkable) home_mod_infos

        debugTraceMsg dflags 3 (text "link: linkables are ..." $$ vcat (map ppr linkables))

        -- check for the -no-link flag
        if isNoLink (ghcLink dflags)
          then do debugTraceMsg dflags 3 (text "link(batch): linking omitted (-c flag given).")
                  return Succeeded
          else do

        let getOfiles (LM _ _ us) = map nameOfObject (filter isObject us)
            obj_files = concatMap getOfiles linkables

            exe_file = exeFileName staticLink dflags

        linking_needed <- linkingNeeded dflags staticLink linkables pkg_deps

        if not (gopt Opt_ForceRecomp dflags) && not linking_needed
           then do debugTraceMsg dflags 2 (text exe_file <+> text "is up to date, linking not required.")
                   return Succeeded
           else do

        compilationProgressMsg dflags ("Linking " ++ exe_file ++ " ...")

        -- Don't showPass in Batch mode; doLink will do that for us.
        let link = case ghcLink dflags of
                LinkBinary    -> linkBinary
                LinkStaticLib -> linkStaticLibCheck
                LinkDynLib    -> linkDynLibCheck
                other         -> panicBadLink other
        link dflags obj_files pkg_deps

        debugTraceMsg dflags 3 (text "link: done")

        -- linkBinary only returns if it succeeds
        return Succeeded

   | otherwise
   = do debugTraceMsg dflags 3 (text "link(batch): upsweep (partially) failed OR" $$
                                text "   Main.main not exported; not linking.")
        return Succeeded


linkingNeeded :: DynFlags -> Bool -> [Linkable] -> [InstalledUnitId] -> IO Bool
linkingNeeded dflags staticLink linkables pkg_deps = do
        -- if the modification time on the executable is later than the
        -- modification times on all of the objects and libraries, then omit
        -- linking (unless the -fforce-recomp flag was given).
  let exe_file = exeFileName staticLink dflags
  e_exe_time <- tryIO $ getModificationUTCTime exe_file
  case e_exe_time of
    Left _  -> return True
    Right t -> do
        -- first check object files and extra_ld_inputs
        let extra_ld_inputs = [ f | FileOption _ f <- ldInputs dflags ]
        e_extra_times <- mapM (tryIO . getModificationUTCTime) extra_ld_inputs
        let (errs,extra_times) = splitEithers e_extra_times
        let obj_times =  map linkableTime linkables ++ extra_times
        if not (null errs) || any (t <) obj_times
            then return True
            else do

        -- next, check libraries. XXX this only checks Haskell libraries,
        -- not extra_libraries or -l things from the command line.
        let pkg_hslibs  = [ (collectLibraryPaths dflags [c], lib)
                          | Just c <- map (lookupInstalledPackage dflags) pkg_deps,
                            lib <- packageHsLibs dflags c ]

        pkg_libfiles <- mapM (uncurry (findHSLib dflags)) pkg_hslibs
        if any isNothing pkg_libfiles then return True else do
        e_lib_times <- mapM (tryIO . getModificationUTCTime)
                          (catMaybes pkg_libfiles)
        let (lib_errs,lib_times) = splitEithers e_lib_times
        if not (null lib_errs) || any (t <) lib_times
           then return True
           else checkLinkInfo dflags pkg_deps exe_file

-- Returns 'False' if it was, and we can avoid linking, because the
-- previous binary was linked with "the same options".
checkLinkInfo :: DynFlags -> [InstalledUnitId] -> FilePath -> IO Bool
checkLinkInfo dflags pkg_deps exe_file
 | not (platformSupportsSavingLinkOpts (platformOS (targetPlatform dflags)))
 -- ToDo: Windows and OS X do not use the ELF binary format, so
 -- readelf does not work there.  We need to find another way to do
 -- this.
 = return False -- conservatively we should return True, but not
                -- linking in this case was the behaviour for a long
                -- time so we leave it as-is.
 | otherwise
 = do
   link_info <- getLinkInfo dflags pkg_deps
   debugTraceMsg dflags 3 $ text ("Link info: " ++ link_info)
   m_exe_link_info <- readElfNoteAsString dflags exe_file
                          ghcLinkInfoSectionName ghcLinkInfoNoteName
   let sameLinkInfo = (Just link_info == m_exe_link_info)
   debugTraceMsg dflags 3 $ case m_exe_link_info of
     Nothing -> text "Exe link info: Not found"
     Just s
       | sameLinkInfo -> text ("Exe link info is the same")
       | otherwise    -> text ("Exe link info is different: " ++ s)
   return (not sameLinkInfo)

platformSupportsSavingLinkOpts :: OS -> Bool
platformSupportsSavingLinkOpts os
  | os == OSSolaris2 = False -- see #5382
  | otherwise        = osElfTarget os

-- See Note [LinkInfo section]
ghcLinkInfoSectionName :: String
ghcLinkInfoSectionName = ".debug-ghc-link-info"
   -- if we use the ".debug" prefix, then strip will strip it by default

-- Identifier for the note (see Note [LinkInfo section])
ghcLinkInfoNoteName :: String
ghcLinkInfoNoteName = "GHC link info"

findHSLib :: DynFlags -> [String] -> String -> IO (Maybe FilePath)
findHSLib dflags dirs lib = do
  let batch_lib_file = if WayDyn `notElem` ways dflags
                       then "lib" ++ lib <.> "a"
                       else mkSOName (targetPlatform dflags) lib
  found <- filterM doesFileExist (map (</> batch_lib_file) dirs)
  case found of
    [] -> return Nothing
    (x:_) -> return (Just x)

-- -----------------------------------------------------------------------------
-- Compile files in one-shot mode.

oneShot :: HscEnv -> Phase -> [(String, Maybe Phase)] -> IO ()
oneShot hsc_env stop_phase srcs = do
  o_files <- mapM (compileFile hsc_env stop_phase) srcs
  doLink (hsc_dflags hsc_env) stop_phase o_files

compileFile :: HscEnv -> Phase -> (FilePath, Maybe Phase) -> IO FilePath
compileFile hsc_env stop_phase (src, mb_phase) = do
   exists <- doesFileExist src
   when (not exists) $
        throwGhcExceptionIO (CmdLineError ("does not exist: " ++ src))

   let
        dflags    = hsc_dflags hsc_env
        split     = gopt Opt_SplitObjs dflags
        mb_o_file = outputFile dflags
        ghc_link  = ghcLink dflags      -- Set by -c or -no-link

        -- When linking, the -o argument refers to the linker's output.
        -- otherwise, we use it as the name for the pipeline's output.
        output
         -- If we are dong -fno-code, then act as if the output is
         -- 'Temporary'. This stops GHC trying to copy files to their
         -- final location.
         | HscNothing <- hscTarget dflags = Temporary
         | StopLn <- stop_phase, not (isNoLink ghc_link) = Persistent
                -- -o foo applies to linker
         | isJust mb_o_file = SpecificFile
                -- -o foo applies to the file we are compiling now
         | otherwise = Persistent

        stop_phase' = case stop_phase of
                        As _ | split -> SplitAs
                        _            -> stop_phase

   ( _, out_file) <- runPipeline stop_phase' hsc_env
                            (src, fmap RealPhase mb_phase) Nothing output
                            Nothing{-no ModLocation-} Nothing
   return out_file


doLink :: DynFlags -> Phase -> [FilePath] -> IO ()
doLink dflags stop_phase o_files
  | not (isStopLn stop_phase)
  = return ()           -- We stopped before the linking phase

  | otherwise
  = case ghcLink dflags of
        NoLink        -> return ()
        LinkBinary    -> linkBinary         dflags o_files []
        LinkStaticLib -> linkStaticLibCheck dflags o_files []
        LinkDynLib    -> linkDynLibCheck    dflags o_files []
        other         -> panicBadLink other


-- ---------------------------------------------------------------------------

-- | Run a compilation pipeline, consisting of multiple phases.
--
-- This is the interface to the compilation pipeline, which runs
-- a series of compilation steps on a single source file, specifying
-- at which stage to stop.
--
-- The DynFlags can be modified by phases in the pipeline (eg. by
-- OPTIONS_GHC pragmas), and the changes affect later phases in the
-- pipeline.
runPipeline
  :: Phase                      -- ^ When to stop
  -> HscEnv                     -- ^ Compilation environment
  -> (FilePath,Maybe PhasePlus) -- ^ Input filename (and maybe -x suffix)
  -> Maybe FilePath             -- ^ original basename (if different from ^^^)
  -> PipelineOutput             -- ^ Output filename
  -> Maybe ModLocation          -- ^ A ModLocation, if this is a Haskell module
  -> Maybe FilePath             -- ^ stub object, if we have one
  -> IO (DynFlags, FilePath)    -- ^ (final flags, output filename)
runPipeline stop_phase hsc_env0 (input_fn, mb_phase)
             mb_basename output maybe_loc maybe_stub_o

    = do let
             dflags0 = hsc_dflags hsc_env0

             -- Decide where dump files should go based on the pipeline output
             dflags = dflags0 { dumpPrefix = Just (basename ++ ".") }
             hsc_env = hsc_env0 {hsc_dflags = dflags}

             (input_basename, suffix) = splitExtension input_fn
             suffix' = drop 1 suffix -- strip off the .
             basename | Just b <- mb_basename = b
                      | otherwise             = input_basename

             -- If we were given a -x flag, then use that phase to start from
             start_phase = fromMaybe (RealPhase (startPhase suffix')) mb_phase

             isHaskell (RealPhase (Unlit _)) = True
             isHaskell (RealPhase (Cpp   _)) = True
             isHaskell (RealPhase (HsPp  _)) = True
             isHaskell (RealPhase (Hsc   _)) = True
             isHaskell (HscOut {})           = True
             isHaskell _                     = False

             isHaskellishFile = isHaskell start_phase

             env = PipeEnv{ stop_phase,
                            src_filename = input_fn,
                            src_basename = basename,
                            src_suffix = suffix',
                            output_spec = output }

         when (isBackpackishSuffix suffix') $
           throwGhcExceptionIO (UsageError
                       ("use --backpack to process " ++ input_fn))

         -- We want to catch cases of "you can't get there from here" before
         -- we start the pipeline, because otherwise it will just run off the
         -- end.
         let happensBefore' = happensBefore dflags
         case start_phase of
             RealPhase start_phase' ->
                 -- See Note [Partial ordering on phases]
                 -- Not the same as: (stop_phase `happensBefore` start_phase')
                 when (not (start_phase' `happensBefore'` stop_phase ||
                            start_phase' `eqPhase` stop_phase)) $
                       throwGhcExceptionIO (UsageError
                                   ("cannot compile this file to desired target: "
                                      ++ input_fn))
             HscOut {} -> return ()

         debugTraceMsg dflags 4 (text "Running the pipeline")
         r <- runPipeline' start_phase hsc_env env input_fn
                           maybe_loc maybe_stub_o

         -- If we are compiling a Haskell module, and doing
         -- -dynamic-too, but couldn't do the -dynamic-too fast
         -- path, then rerun the pipeline for the dyn way
         let dflags = hsc_dflags hsc_env
         -- NB: Currently disabled on Windows (ref #7134, #8228, and #5987)
         when (not $ platformOS (targetPlatform dflags) == OSMinGW32) $ do
           when isHaskellishFile $ whenCannotGenerateDynamicToo dflags $ do
               debugTraceMsg dflags 4
                   (text "Running the pipeline again for -dynamic-too")
               let dflags' = dynamicTooMkDynamicDynFlags dflags
               hsc_env' <- newHscEnv dflags'
               _ <- runPipeline' start_phase hsc_env' env input_fn
                                 maybe_loc maybe_stub_o
               return ()
         return r

runPipeline'
  :: PhasePlus                  -- ^ When to start
  -> HscEnv                     -- ^ Compilation environment
  -> PipeEnv
  -> FilePath                   -- ^ Input filename
  -> Maybe ModLocation          -- ^ A ModLocation, if this is a Haskell module
  -> Maybe FilePath             -- ^ stub object, if we have one
  -> IO (DynFlags, FilePath)    -- ^ (final flags, output filename)
runPipeline' start_phase hsc_env env input_fn
             maybe_loc maybe_stub_o
  = do
  -- Execute the pipeline...
  let state = PipeState{ hsc_env, maybe_loc, maybe_stub_o = maybe_stub_o }

  evalP (pipeLoop start_phase input_fn) env state

-- ---------------------------------------------------------------------------
-- outer pipeline loop

-- | pipeLoop runs phases until we reach the stop phase
pipeLoop :: PhasePlus -> FilePath -> CompPipeline (DynFlags, FilePath)
pipeLoop phase input_fn = do
  env <- getPipeEnv
  dflags <- getDynFlags
  -- See Note [Partial ordering on phases]
  let happensBefore' = happensBefore dflags
      stopPhase = stop_phase env
  case phase of
   RealPhase realPhase | realPhase `eqPhase` stopPhase            -- All done
     -> -- Sometimes, a compilation phase doesn't actually generate any output
        -- (eg. the CPP phase when -fcpp is not turned on).  If we end on this
        -- stage, but we wanted to keep the output, then we have to explicitly
        -- copy the file, remembering to prepend a {-# LINE #-} pragma so that
        -- further compilation stages can tell what the original filename was.
        case output_spec env of
        Temporary ->
            return (dflags, input_fn)
        output ->
            do pst <- getPipeState
               final_fn <- liftIO $ getOutputFilename
                                        stopPhase output (src_basename env)
                                        dflags stopPhase (maybe_loc pst)
               when (final_fn /= input_fn) $ do
                  let msg = ("Copying `" ++ input_fn ++"' to `" ++ final_fn ++ "'")
                      line_prag = Just ("{-# LINE 1 \"" ++ src_filename env ++ "\" #-}\n")
                  liftIO $ copyWithHeader dflags msg line_prag input_fn final_fn
               return (dflags, final_fn)


     | not (realPhase `happensBefore'` stopPhase)
        -- Something has gone wrong.  We'll try to cover all the cases when
        -- this could happen, so if we reach here it is a panic.
        -- eg. it might happen if the -C flag is used on a source file that
        -- has {-# OPTIONS -fasm #-}.
     -> panic ("pipeLoop: at phase " ++ show realPhase ++
           " but I wanted to stop at phase " ++ show stopPhase)

   _
     -> do liftIO $ debugTraceMsg dflags 4
                                  (text "Running phase" <+> ppr phase)
           (next_phase, output_fn) <- runHookedPhase phase input_fn dflags
           r <- pipeLoop next_phase output_fn
           case phase of
               HscOut {} ->
                   whenGeneratingDynamicToo dflags $ do
                       setDynFlags $ dynamicTooMkDynamicDynFlags dflags
                       -- TODO shouldn't ignore result:
                       _ <- pipeLoop phase input_fn
                       return ()
               _ ->
                   return ()
           return r

runHookedPhase :: PhasePlus -> FilePath -> DynFlags
               -> CompPipeline (PhasePlus, FilePath)
runHookedPhase pp input dflags =
  lookupHook runPhaseHook runPhase dflags pp input dflags

-- -----------------------------------------------------------------------------
-- In each phase, we need to know into what filename to generate the
-- output.  All the logic about which filenames we generate output
-- into is embodied in the following function.

-- | Computes the next output filename after we run @next_phase@.
-- Like 'getOutputFilename', but it operates in the 'CompPipeline' monad
-- (which specifies all of the ambient information.)
phaseOutputFilename :: Phase{-next phase-} -> CompPipeline FilePath
phaseOutputFilename next_phase = do
  PipeEnv{stop_phase, src_basename, output_spec} <- getPipeEnv
  PipeState{maybe_loc, hsc_env} <- getPipeState
  let dflags = hsc_dflags hsc_env
  liftIO $ getOutputFilename stop_phase output_spec
                             src_basename dflags next_phase maybe_loc

-- | Computes the next output filename for something in the compilation
-- pipeline.  This is controlled by several variables:
--
--      1. 'Phase': the last phase to be run (e.g. 'stopPhase').  This
--         is used to tell if we're in the last phase or not, because
--         in that case flags like @-o@ may be important.
--      2. 'PipelineOutput': is this intended to be a 'Temporary' or
--         'Persistent' build output?  Temporary files just go in
--         a fresh temporary name.
--      3. 'String': what was the basename of the original input file?
--      4. 'DynFlags': the obvious thing
--      5. 'Phase': the phase we want to determine the output filename of.
--      6. @Maybe ModLocation@: the 'ModLocation' of the module we're
--         compiling; this can be used to override the default output
--         of an object file.  (TODO: do we actually need this?)
getOutputFilename
  :: Phase -> PipelineOutput -> String
  -> DynFlags -> Phase{-next phase-} -> Maybe ModLocation -> IO FilePath
getOutputFilename stop_phase output basename dflags next_phase maybe_location
 | is_last_phase, Persistent   <- output = persistent_fn
 | is_last_phase, SpecificFile <- output = case outputFile dflags of
                                           Just f -> return f
                                           Nothing ->
                                               panic "SpecificFile: No filename"
 | keep_this_output                      = persistent_fn
 | otherwise                             = newTempName dflags suffix
    where
          hcsuf      = hcSuf dflags
          odir       = objectDir dflags
          osuf       = objectSuf dflags
          keep_hc    = gopt Opt_KeepHcFiles dflags
          keep_s     = gopt Opt_KeepSFiles dflags
          keep_bc    = gopt Opt_KeepLlvmFiles dflags

          myPhaseInputExt HCc       = hcsuf
          myPhaseInputExt MergeStub = osuf
          myPhaseInputExt StopLn    = osuf
          myPhaseInputExt other     = phaseInputExt other

          is_last_phase = next_phase `eqPhase` stop_phase

          -- sometimes, we keep output from intermediate stages
          keep_this_output =
               case next_phase of
                       As _    | keep_s     -> True
                       LlvmOpt | keep_bc    -> True
                       HCc     | keep_hc    -> True
                       _other               -> False

          suffix = myPhaseInputExt next_phase

          -- persistent object files get put in odir
          persistent_fn
             | StopLn <- next_phase = return odir_persistent
             | otherwise            = return persistent

          persistent = basename <.> suffix

          odir_persistent
             | Just loc <- maybe_location = ml_obj_file loc
             | Just d <- odir = d </> persistent
             | otherwise      = persistent

-- -----------------------------------------------------------------------------
-- | Each phase in the pipeline returns the next phase to execute, and the
-- name of the file in which the output was placed.
--
-- We must do things dynamically this way, because we often don't know
-- what the rest of the phases will be until part-way through the
-- compilation: for example, an {-# OPTIONS -fasm #-} at the beginning
-- of a source file can change the latter stages of the pipeline from
-- taking the LLVM route to using the native code generator.
--
runPhase :: PhasePlus   -- ^ Run this phase
         -> FilePath    -- ^ name of the input file
         -> DynFlags    -- ^ for convenience, we pass the current dflags in
         -> CompPipeline (PhasePlus,           -- next phase to run
                          FilePath)            -- output filename

        -- Invariant: the output filename always contains the output
        -- Interesting case: Hsc when there is no recompilation to do
        --                   Then the output filename is still a .o file


-------------------------------------------------------------------------------
-- Unlit phase

runPhase (RealPhase (Unlit sf)) input_fn dflags
  = do
       output_fn <- phaseOutputFilename (Cpp sf)

       let flags = [ -- The -h option passes the file name for unlit to
                     -- put in a #line directive
                     SysTools.Option     "-h"
                     -- See Note [Don't normalise input filenames].
                   , SysTools.Option $ escape input_fn
                   , SysTools.FileOption "" input_fn
                   , SysTools.FileOption "" output_fn
                   ]

       liftIO $ SysTools.runUnlit dflags flags

       return (RealPhase (Cpp sf), output_fn)
  where
       -- escape the characters \, ", and ', but don't try to escape
       -- Unicode or anything else (so we don't use Util.charToC
       -- here).  If we get this wrong, then in
       -- Coverage.isGoodTickSrcSpan where we check that the filename in
       -- a SrcLoc is the same as the source filenaame, the two will
       -- look bogusly different. See test:
       -- libraries/hpc/tests/function/subdir/tough2.hs
       escape ('\\':cs) = '\\':'\\': escape cs
       escape ('\"':cs) = '\\':'\"': escape cs
       escape ('\'':cs) = '\\':'\'': escape cs
       escape (c:cs)    = c : escape cs
       escape []        = []

-------------------------------------------------------------------------------
-- Cpp phase : (a) gets OPTIONS out of file
--             (b) runs cpp if necessary

runPhase (RealPhase (Cpp sf)) input_fn dflags0
  = do
       src_opts <- liftIO $ getOptionsFromFile dflags0 input_fn
       (dflags1, unhandled_flags, warns)
           <- liftIO $ parseDynamicFilePragma dflags0 src_opts
       setDynFlags dflags1
       liftIO $ checkProcessArgsResult dflags1 unhandled_flags

       if not (xopt LangExt.Cpp dflags1) then do
           -- we have to be careful to emit warnings only once.
           unless (gopt Opt_Pp dflags1) $
               liftIO $ handleFlagWarnings dflags1 warns

           -- no need to preprocess CPP, just pass input file along
           -- to the next phase of the pipeline.
           return (RealPhase (HsPp sf), input_fn)
        else do
            output_fn <- phaseOutputFilename (HsPp sf)
            liftIO $ doCpp dflags1 True{-raw-}
                           input_fn output_fn
            -- re-read the pragmas now that we've preprocessed the file
            -- See #2464,#3457
            src_opts <- liftIO $ getOptionsFromFile dflags0 output_fn
            (dflags2, unhandled_flags, warns)
                <- liftIO $ parseDynamicFilePragma dflags0 src_opts
            liftIO $ checkProcessArgsResult dflags2 unhandled_flags
            unless (gopt Opt_Pp dflags2) $
                liftIO $ handleFlagWarnings dflags2 warns
            -- the HsPp pass below will emit warnings

            setDynFlags dflags2

            return (RealPhase (HsPp sf), output_fn)

-------------------------------------------------------------------------------
-- HsPp phase

runPhase (RealPhase (HsPp sf)) input_fn dflags
  = do
       if not (gopt Opt_Pp dflags) then
           -- no need to preprocess, just pass input file along
           -- to the next phase of the pipeline.
          return (RealPhase (Hsc sf), input_fn)
        else do
            PipeEnv{src_basename, src_suffix} <- getPipeEnv
            let orig_fn = src_basename <.> src_suffix
            output_fn <- phaseOutputFilename (Hsc sf)
            liftIO $ SysTools.runPp dflags
                           ( [ SysTools.Option     orig_fn
                             , SysTools.Option     input_fn
                             , SysTools.FileOption "" output_fn
                             ]
                           )

            -- re-read pragmas now that we've parsed the file (see #3674)
            src_opts <- liftIO $ getOptionsFromFile dflags output_fn
            (dflags1, unhandled_flags, warns)
                <- liftIO $ parseDynamicFilePragma dflags src_opts
            setDynFlags dflags1
            liftIO $ checkProcessArgsResult dflags1 unhandled_flags
            liftIO $ handleFlagWarnings dflags1 warns

            return (RealPhase (Hsc sf), output_fn)

-----------------------------------------------------------------------------
-- Hsc phase

-- Compilation of a single module, in "legacy" mode (_not_ under
-- the direction of the compilation manager).
runPhase (RealPhase (Hsc src_flavour)) input_fn dflags0
 = do   -- normal Hsc mode, not mkdependHS

        PipeEnv{ stop_phase=stop,
                 src_basename=basename,
                 src_suffix=suff } <- getPipeEnv

  -- we add the current directory (i.e. the directory in which
  -- the .hs files resides) to the include path, since this is
  -- what gcc does, and it's probably what you want.
        let current_dir = takeDirectory basename
            paths = includePaths dflags0
            dflags = dflags0 { includePaths = current_dir : paths }

        setDynFlags dflags

  -- gather the imports and module name
        (hspp_buf,mod_name,imps,src_imps) <- liftIO $ do
          do
            buf <- hGetStringBuffer input_fn
            (src_imps,imps,L _ mod_name) <- getImports dflags buf input_fn (basename <.> suff)
            return (Just buf, mod_name, imps, src_imps)

  -- Take -o into account if present
  -- Very like -ohi, but we must *only* do this if we aren't linking
  -- (If we're linking then the -o applies to the linked thing, not to
  -- the object file for one module.)
  -- Note the nasty duplication with the same computation in compileFile above
        location <- getLocation src_flavour mod_name

        let o_file = ml_obj_file location -- The real object file
            hi_file = ml_hi_file location
            dest_file | writeInterfaceOnlyMode dflags
                            = hi_file
                      | otherwise
                            = o_file

  -- Figure out if the source has changed, for recompilation avoidance.
  --
  -- Setting source_unchanged to True means that M.o seems
  -- to be up to date wrt M.hs; so no need to recompile unless imports have
  -- changed (which the compiler itself figures out).
  -- Setting source_unchanged to False tells the compiler that M.o is out of
  -- date wrt M.hs (or M.o doesn't exist) so we must recompile regardless.
        src_timestamp <- liftIO $ getModificationUTCTime (basename <.> suff)

        source_unchanged <- liftIO $
          if not (isStopLn stop)
                -- SourceModified unconditionally if
                --      (a) recompilation checker is off, or
                --      (b) we aren't going all the way to .o file (e.g. ghc -S)
             then return SourceModified
                -- Otherwise look at file modification dates
             else do dest_file_exists <- doesFileExist dest_file
                     if not dest_file_exists
                        then return SourceModified       -- Need to recompile
                        else do t2 <- getModificationUTCTime dest_file
                                if t2 > src_timestamp
                                  then return SourceUnmodified
                                  else return SourceModified

        PipeState{hsc_env=hsc_env'} <- getPipeState

  -- Tell the finder cache about this module
        mod <- liftIO $ addHomeModuleToFinder hsc_env' mod_name location

  -- Make the ModSummary to hand to hscMain
        let
            mod_summary = ModSummary {  ms_mod       = mod,
                                        ms_hsc_src   = src_flavour,
                                        ms_hspp_file = input_fn,
                                        ms_hspp_opts = dflags,
                                        ms_hspp_buf  = hspp_buf,
                                        ms_location  = location,
                                        ms_hs_date   = src_timestamp,
                                        ms_obj_date  = Nothing,
                                        ms_parsed_mod   = Nothing,
                                        ms_iface_date   = Nothing,
                                        ms_textual_imps = imps,
                                        ms_srcimps      = src_imps }

  -- run the compiler!
        let msg hsc_env _ what _ = oneShotMsg hsc_env what
        (result, _) <- liftIO $ hscIncrementalCompile True Nothing (Just msg) hsc_env'
                            mod_summary source_unchanged Nothing (1,1)

        return (HscOut src_flavour mod_name result,
                panic "HscOut doesn't have an input filename")

runPhase (HscOut src_flavour mod_name result) _ dflags = do
        location <- getLocation src_flavour mod_name
        setModLocation location

        let o_file = ml_obj_file location -- The real object file
            hsc_lang = hscTarget dflags
            next_phase = hscPostBackendPhase dflags src_flavour hsc_lang

        case result of
            HscNotGeneratingCode ->
                return (RealPhase StopLn,
                        panic "No output filename from Hsc when no-code")
            HscUpToDate ->
                do liftIO $ touchObjectFile dflags o_file
                   -- The .o file must have a later modification date
                   -- than the source file (else we wouldn't get Nothing)
                   -- but we touch it anyway, to keep 'make' happy (we think).
                   return (RealPhase StopLn, o_file)
            HscUpdateBoot ->
                do -- In the case of hs-boot files, generate a dummy .o-boot
                   -- stamp file for the benefit of Make
                   liftIO $ touchObjectFile dflags o_file
                   return (RealPhase StopLn, o_file)
            HscUpdateSig ->
                do -- We need to create a REAL but empty .o file
                   -- because we are going to attempt to put it in a library
                   PipeState{hsc_env=hsc_env'} <- getPipeState
                   let input_fn = expectJust "runPhase" (ml_hs_file location)
                       basename = dropExtension input_fn
                   liftIO $ compileEmptyStub dflags hsc_env' basename location mod_name
                   return (RealPhase StopLn, o_file)
            HscRecomp cgguts mod_summary
              -> do output_fn <- phaseOutputFilename next_phase

                    PipeState{hsc_env=hsc_env'} <- getPipeState

                    (outputFilename, mStub) <- liftIO $ hscGenHardCode hsc_env' cgguts mod_summary output_fn
                    case mStub of
                        Nothing -> return ()
                        Just stub_c ->
                            do stub_o <- liftIO $ compileStub hsc_env' stub_c
                               setStubO stub_o

                    return (RealPhase next_phase, outputFilename)

-----------------------------------------------------------------------------
-- Cmm phase

runPhase (RealPhase CmmCpp) input_fn dflags
  = do
       output_fn <- phaseOutputFilename Cmm
       liftIO $ doCpp dflags False{-not raw-}
                      input_fn output_fn
       return (RealPhase Cmm, output_fn)

runPhase (RealPhase Cmm) input_fn dflags
  = do
        let hsc_lang = hscTarget dflags

        let next_phase = hscPostBackendPhase dflags HsSrcFile hsc_lang

        output_fn <- phaseOutputFilename next_phase

        PipeState{hsc_env} <- getPipeState

        liftIO $ hscCompileCmmFile hsc_env input_fn output_fn

        return (RealPhase next_phase, output_fn)

-----------------------------------------------------------------------------
-- Cc phase

-- we don't support preprocessing .c files (with -E) now.  Doing so introduces
-- way too many hacks, and I can't say I've ever used it anyway.

runPhase (RealPhase cc_phase) input_fn dflags
   | any (cc_phase `eqPhase`) [Cc, Ccxx, HCc, Cobjc, Cobjcxx]
   = do
        let platform = targetPlatform dflags
            hcc = cc_phase `eqPhase` HCc

        let cmdline_include_paths = includePaths dflags

        -- HC files have the dependent packages stamped into them
        pkgs <- if hcc then liftIO $ getHCFilePackages input_fn else return []

        -- add package include paths even if we're just compiling .c
        -- files; this is the Value Add(TM) that using ghc instead of
        -- gcc gives you :)
        pkg_include_dirs <- liftIO $ getPackageIncludePath dflags pkgs
        let include_paths = foldr (\ x xs -> ("-I" ++ x) : xs) []
                              (cmdline_include_paths ++ pkg_include_dirs)

        let gcc_extra_viac_flags = extraGccViaCFlags dflags
        let pic_c_flags = picCCOpts dflags

        let verbFlags = getVerbFlags dflags

        -- cc-options are not passed when compiling .hc files.  Our
        -- hc code doesn't not #include any header files anyway, so these
        -- options aren't necessary.
        pkg_extra_cc_opts <- liftIO $
          if cc_phase `eqPhase` HCc
             then return []
             else getPackageExtraCcOpts dflags pkgs

        framework_paths <-
            if platformUsesFrameworks platform
            then do pkgFrameworkPaths <- liftIO $ getPackageFrameworkPath dflags pkgs
                    let cmdlineFrameworkPaths = frameworkPaths dflags
                    return $ map ("-F"++)
                                 (cmdlineFrameworkPaths ++ pkgFrameworkPaths)
            else return []

        let split_objs = gopt Opt_SplitObjs dflags
            split_opt | hcc && split_objs = [ "-DUSE_SPLIT_MARKERS" ]
                      | otherwise         = [ ]

        let cc_opt | optLevel dflags >= 2 = [ "-O2" ]
                   | optLevel dflags >= 1 = [ "-O" ]
                   | otherwise            = []

        -- Decide next phase
        let next_phase = As False
        output_fn <- phaseOutputFilename next_phase

        let
          more_hcc_opts =
                -- on x86 the floating point regs have greater precision
                -- than a double, which leads to unpredictable results.
                -- By default, we turn this off with -ffloat-store unless
                -- the user specified -fexcess-precision.
                (if platformArch platform == ArchX86 &&
                    not (gopt Opt_ExcessPrecision dflags)
                        then [ "-ffloat-store" ]
                        else []) ++

                -- gcc's -fstrict-aliasing allows two accesses to memory
                -- to be considered non-aliasing if they have different types.
                -- This interacts badly with the C code we generate, which is
                -- very weakly typed, being derived from C--.
                ["-fno-strict-aliasing"]

        ghcVersionH <- liftIO $ getGhcVersionPathName dflags

        let gcc_lang_opt | cc_phase `eqPhase` Ccxx    = "c++"
                         | cc_phase `eqPhase` Cobjc   = "objective-c"
                         | cc_phase `eqPhase` Cobjcxx = "objective-c++"
                         | otherwise                  = "c"
        liftIO $ SysTools.runCc dflags (
                -- force the C compiler to interpret this file as C when
                -- compiling .hc files, by adding the -x c option.
                -- Also useful for plain .c files, just in case GHC saw a
                -- -x c option.
                        [ SysTools.Option "-x", SysTools.Option gcc_lang_opt
                        , SysTools.FileOption "" input_fn
                        , SysTools.Option "-o"
                        , SysTools.FileOption "" output_fn
                        ]
                       ++ map SysTools.Option (
                          pic_c_flags

                -- Stub files generated for foreign exports references the runIO_closure
                -- and runNonIO_closure symbols, which are defined in the base package.
                -- These symbols are imported into the stub.c file via RtsAPI.h, and the
                -- way we do the import depends on whether we're currently compiling
                -- the base package or not.
                       ++ (if platformOS platform == OSMinGW32 &&
                              thisPackage dflags == baseUnitId
                                then [ "-DCOMPILING_BASE_PACKAGE" ]
                                else [])

        -- We only support SparcV9 and better because V8 lacks an atomic CAS
        -- instruction. Note that the user can still override this
        -- (e.g., -mcpu=ultrasparc) as GCC picks the "best" -mcpu flag
        -- regardless of the ordering.
        --
        -- This is a temporary hack. See #2872, commit
        -- 5bd3072ac30216a505151601884ac88bf404c9f2
                       ++ (if platformArch platform == ArchSPARC
                           then ["-mcpu=v9"]
                           else [])

                       -- GCC 4.6+ doesn't like -Wimplicit when compiling C++.
                       ++ (if (cc_phase /= Ccxx && cc_phase /= Cobjcxx)
                             then ["-Wimplicit"]
                             else [])

                       ++ (if hcc
                             then gcc_extra_viac_flags ++ more_hcc_opts
                             else [])
                       ++ verbFlags
                       ++ [ "-S" ]
                       ++ cc_opt
                       ++ [ "-include", ghcVersionH ]
                       ++ framework_paths
                       ++ split_opt
                       ++ include_paths
                       ++ pkg_extra_cc_opts
                       ))

        return (RealPhase next_phase, output_fn)

-----------------------------------------------------------------------------
-- Splitting phase

runPhase (RealPhase Splitter) input_fn dflags
  = do  -- tmp_pfx is the prefix used for the split .s files

        split_s_prefix <- liftIO $ SysTools.newTempName dflags "split"
        let n_files_fn = split_s_prefix

        liftIO $ SysTools.runSplit dflags
                          [ SysTools.FileOption "" input_fn
                          , SysTools.FileOption "" split_s_prefix
                          , SysTools.FileOption "" n_files_fn
                          ]

        -- Save the number of split files for future references
        s <- liftIO $ readFile n_files_fn
        let n_files = read s :: Int
            dflags' = dflags { splitInfo = Just (split_s_prefix, n_files) }

        setDynFlags dflags'

        -- Remember to delete all these files
        liftIO $ addFilesToClean dflags'
                                 [ split_s_prefix ++ "__" ++ show n ++ ".s"
                                 | n <- [1..n_files]]

        return (RealPhase SplitAs,
                "**splitter**") -- we don't use the filename in SplitAs

-----------------------------------------------------------------------------
-- As, SpitAs phase : Assembler

-- This is for calling the assembler on a regular assembly file (not split).
runPhase (RealPhase (As with_cpp)) input_fn dflags
  = do
        -- LLVM from version 3.0 onwards doesn't support the OS X system
        -- assembler, so we use clang as the assembler instead. (#5636)
        let whichAsProg | hscTarget dflags == HscLlvm &&
                          platformOS (targetPlatform dflags) == OSDarwin
                        = return SysTools.runClang
                        | otherwise = return SysTools.runAs

        as_prog <- whichAsProg
        let cmdline_include_paths = includePaths dflags
        let pic_c_flags = picCCOpts dflags

        next_phase <- maybeMergeStub
        output_fn <- phaseOutputFilename next_phase

        -- we create directories for the object file, because it
        -- might be a hierarchical module.
        liftIO $ createDirectoryIfMissing True (takeDirectory output_fn)

        ccInfo <- liftIO $ getCompilerInfo dflags
        let runAssembler inputFilename outputFilename
                = liftIO $ as_prog dflags
                       ([ SysTools.Option ("-I" ++ p) | p <- cmdline_include_paths ]

                       -- See Note [-fPIC for assembler]
                       ++ map SysTools.Option pic_c_flags

        -- We only support SparcV9 and better because V8 lacks an atomic CAS
        -- instruction so we have to make sure that the assembler accepts the
        -- instruction set. Note that the user can still override this
        -- (e.g., -mcpu=ultrasparc). GCC picks the "best" -mcpu flag
        -- regardless of the ordering.
        --
        -- This is a temporary hack.
                       ++ (if platformArch (targetPlatform dflags) == ArchSPARC
                           then [SysTools.Option "-mcpu=v9"]
                           else [])
                       ++ (if any (ccInfo ==) [Clang, AppleClang, AppleClang51]
                            then [SysTools.Option "-Qunused-arguments"]
                            else [])
                       ++ [ SysTools.Option "-x"
                          , if with_cpp
                              then SysTools.Option "assembler-with-cpp"
                              else SysTools.Option "assembler"
                          , SysTools.Option "-c"
                          , SysTools.FileOption "" inputFilename
                          , SysTools.Option "-o"
                          , SysTools.FileOption "" outputFilename
                          ])

        liftIO $ debugTraceMsg dflags 4 (text "Running the assembler")
        runAssembler input_fn output_fn
        return (RealPhase next_phase, output_fn)


-- This is for calling the assembler on a split assembly file (so a collection
-- of assembly files)
runPhase (RealPhase SplitAs) _input_fn dflags
  = do
        -- we'll handle the stub_o file in this phase, so don't MergeStub,
        -- just jump straight to StopLn afterwards.
        let next_phase = StopLn
        output_fn <- phaseOutputFilename next_phase

        let base_o = dropExtension output_fn
            osuf = objectSuf dflags
            split_odir  = base_o ++ "_" ++ osuf ++ "_split"

        let pic_c_flags = picCCOpts dflags

        -- this also creates the hierarchy
        liftIO $ createDirectoryIfMissing True split_odir

        -- remove M_split/ *.o, because we're going to archive M_split/ *.o
        -- later and we don't want to pick up any old objects.
        fs <- liftIO $ getDirectoryContents split_odir
        liftIO $ mapM_ removeFile $
                map (split_odir </>) $ filter (osuf `isSuffixOf`) fs

        let (split_s_prefix, n) = case splitInfo dflags of
                                  Nothing -> panic "No split info"
                                  Just x -> x

        let split_s   n = split_s_prefix ++ "__" ++ show n <.> "s"

            split_obj :: Int -> FilePath
            split_obj n = split_odir </>
                          takeFileName base_o ++ "__" ++ show n <.> osuf

        let assemble_file n
              = SysTools.runAs dflags (

        -- We only support SparcV9 and better because V8 lacks an atomic CAS
        -- instruction so we have to make sure that the assembler accepts the
        -- instruction set. Note that the user can still override this
        -- (e.g., -mcpu=ultrasparc). GCC picks the "best" -mcpu flag
        -- regardless of the ordering.
        --
        -- This is a temporary hack.
                          (if platformArch (targetPlatform dflags) == ArchSPARC
                           then [SysTools.Option "-mcpu=v9"]
                           else []) ++

                          -- See Note [-fPIC for assembler]
                          map SysTools.Option pic_c_flags ++

                          [ SysTools.Option "-c"
                          , SysTools.Option "-o"
                          , SysTools.FileOption "" (split_obj n)
                          , SysTools.FileOption "" (split_s n)
                          ])

        liftIO $ mapM_ assemble_file [1..n]

        -- Note [pipeline-split-init]
        -- If we have a stub file, it may contain constructor
        -- functions for initialisation of this module.  We can't
        -- simply leave the stub as a separate object file, because it
        -- will never be linked in: nothing refers to it.  We need to
        -- ensure that if we ever refer to the data in this module
        -- that needs initialisation, then we also pull in the
        -- initialisation routine.
        --
        -- To that end, we make a DANGEROUS ASSUMPTION here: the data
        -- that needs to be initialised is all in the FIRST split
        -- object.  See Note [codegen-split-init].

        PipeState{maybe_stub_o} <- getPipeState
        case maybe_stub_o of
            Nothing     -> return ()
            Just stub_o -> liftIO $ do
                     tmp_split_1 <- newTempName dflags osuf
                     let split_1 = split_obj 1
                     copyFile split_1 tmp_split_1
                     removeFile split_1
                     joinObjectFiles dflags [tmp_split_1, stub_o] split_1

        -- join them into a single .o file
        liftIO $ joinObjectFiles dflags (map split_obj [1..n]) output_fn

        return (RealPhase next_phase, output_fn)

-----------------------------------------------------------------------------
-- LlvmOpt phase

runPhase (RealPhase LlvmOpt) input_fn dflags
  = do
    let opt_lvl  = max 0 (min 2 $ optLevel dflags)
        -- don't specify anything if user has specified commands. We do this
        -- for opt but not llc since opt is very specifically for optimisation
        -- passes only, so if the user is passing us extra options we assume
        -- they know what they are doing and don't get in the way.
        optFlag  = if null (getOpts dflags opt_lo)
                       then map SysTools.Option $ words (llvmOpts !! opt_lvl)
                       else []
        tbaa | gopt Opt_LlvmTBAA dflags = "--enable-tbaa=true"
             | otherwise                = "--enable-tbaa=false"


    output_fn <- phaseOutputFilename LlvmLlc

    liftIO $ SysTools.runLlvmOpt dflags
               ([ SysTools.FileOption "" input_fn,
                    SysTools.Option "-o",
                    SysTools.FileOption "" output_fn]
                ++ optFlag
                ++ [SysTools.Option tbaa])

    return (RealPhase LlvmLlc, output_fn)
  where
        -- we always (unless -optlo specified) run Opt since we rely on it to
        -- fix up some pretty big deficiencies in the code we generate
        llvmOpts =  [ "-mem2reg -globalopt"
                    , "-O1 -globalopt"
                    , "-O2"
                    ]

-----------------------------------------------------------------------------
-- LlvmLlc phase

runPhase (RealPhase LlvmLlc) input_fn dflags
  = do
    let opt_lvl = max 0 (min 2 $ optLevel dflags)
        -- iOS requires external references to be loaded indirectly from the
        -- DATA segment or dyld traps at runtime writing into TEXT: see #7722
        rmodel | platformOS (targetPlatform dflags) == OSiOS = "dynamic-no-pic"
               | gopt Opt_PIC dflags                         = "pic"
               | WayDyn `elem` ways dflags                   = "dynamic-no-pic"
               | otherwise                                   = "static"
        tbaa | gopt Opt_LlvmTBAA dflags = "--enable-tbaa=true"
             | otherwise                = "--enable-tbaa=false"

    -- hidden debugging flag '-dno-llvm-mangler' to skip mangling
    let next_phase = case gopt Opt_NoLlvmMangler dflags of
                         False                            -> LlvmMangle
                         True | gopt Opt_SplitObjs dflags -> Splitter
                         True                             -> As False

    output_fn <- phaseOutputFilename next_phase

    liftIO $ SysTools.runLlvmLlc dflags
                ([ SysTools.Option (llvmOpts !! opt_lvl),
                    SysTools.Option $ "-relocation-model=" ++ rmodel,
                    SysTools.FileOption "" input_fn,
                    SysTools.Option "-o", SysTools.FileOption "" output_fn]
                ++ [SysTools.Option tbaa]
                ++ map SysTools.Option fpOpts
                ++ map SysTools.Option abiOpts
                ++ map SysTools.Option sseOpts
                ++ map SysTools.Option avxOpts
                ++ map SysTools.Option avx512Opts
                ++ map SysTools.Option stackAlignOpts)

    return (RealPhase next_phase, output_fn)
  where
        -- Bug in LLVM at O3 on OSX.
        llvmOpts = if platformOS (targetPlatform dflags) == OSDarwin
                   then ["-O1", "-O2", "-O2"]
                   else ["-O1", "-O2", "-O3"]
        -- On ARMv7 using LLVM, LLVM fails to allocate floating point registers
        -- while compiling GHC source code. It's probably due to fact that it
        -- does not enable VFP by default. Let's do this manually here
        fpOpts = case platformArch (targetPlatform dflags) of
                   ArchARM ARMv7 ext _ -> if (elem VFPv3 ext)
                                      then ["-mattr=+v7,+vfp3"]
                                      else if (elem VFPv3D16 ext)
                                           then ["-mattr=+v7,+vfp3,+d16"]
                                           else []
                   ArchARM ARMv6 ext _ -> if (elem VFPv2 ext)
                                          then ["-mattr=+v6,+vfp2"]
                                          else ["-mattr=+v6"]
                   _                 -> []
        -- On Ubuntu/Debian with ARM hard float ABI, LLVM's llc still
        -- compiles into soft-float ABI. We need to explicitly set abi
        -- to hard
        abiOpts = case platformArch (targetPlatform dflags) of
                    ArchARM _ _ HARD -> ["-float-abi=hard"]
                    ArchARM _ _ _    -> []
                    _                -> []

        sseOpts | isSse4_2Enabled dflags = ["-mattr=+sse42"]
                | isSse2Enabled dflags   = ["-mattr=+sse2"]
                | isSseEnabled dflags    = ["-mattr=+sse"]
                | otherwise              = []

        avxOpts | isAvx512fEnabled dflags = ["-mattr=+avx512f"]
                | isAvx2Enabled dflags    = ["-mattr=+avx2"]
                | isAvxEnabled dflags     = ["-mattr=+avx"]
                | otherwise               = []

        avx512Opts =
          [ "-mattr=+avx512cd" | isAvx512cdEnabled dflags ] ++
          [ "-mattr=+avx512er" | isAvx512erEnabled dflags ] ++
          [ "-mattr=+avx512pf" | isAvx512pfEnabled dflags ]

        stackAlignOpts =
            case platformArch (targetPlatform dflags) of
              ArchX86_64 | isAvxEnabled dflags -> ["-stack-alignment=32"]
              _                                -> []

-----------------------------------------------------------------------------
-- LlvmMangle phase

runPhase (RealPhase LlvmMangle) input_fn dflags
  = do
      let next_phase = if gopt Opt_SplitObjs dflags then Splitter else As False
      output_fn <- phaseOutputFilename next_phase
      liftIO $ llvmFixupAsm dflags input_fn output_fn
      return (RealPhase next_phase, output_fn)

-----------------------------------------------------------------------------
-- merge in stub objects

runPhase (RealPhase MergeStub) input_fn dflags
 = do
     PipeState{maybe_stub_o} <- getPipeState
     output_fn <- phaseOutputFilename StopLn
     liftIO $ createDirectoryIfMissing True (takeDirectory output_fn)
     case maybe_stub_o of
       Nothing ->
         panic "runPhase(MergeStub): no stub"
       Just stub_o -> do
         liftIO $ joinObjectFiles dflags [input_fn, stub_o] output_fn
         return (RealPhase StopLn, output_fn)

-- warning suppression
runPhase (RealPhase other) _input_fn _dflags =
   panic ("runPhase: don't know how to run phase " ++ show other)

maybeMergeStub :: CompPipeline Phase
maybeMergeStub
 = do
     PipeState{maybe_stub_o} <- getPipeState
     if isJust maybe_stub_o then return MergeStub else return StopLn

getLocation :: HscSource -> ModuleName -> CompPipeline ModLocation
getLocation src_flavour mod_name = do
    dflags <- getDynFlags

    PipeEnv{ src_basename=basename,
             src_suffix=suff } <- getPipeEnv

    -- Build a ModLocation to pass to hscMain.
    -- The source filename is rather irrelevant by now, but it's used
    -- by hscMain for messages.  hscMain also needs
    -- the .hi and .o filenames, and this is as good a way
    -- as any to generate them, and better than most. (e.g. takes
    -- into account the -osuf flags)
    location1 <- liftIO $ mkHomeModLocation2 dflags mod_name basename suff

    -- Boot-ify it if necessary
    let location2 | HsBootFile <- src_flavour = addBootSuffixLocn location1
                  | otherwise                 = location1


    -- Take -ohi into account if present
    -- This can't be done in mkHomeModuleLocation because
    -- it only applies to the module being compiles
    let ohi = outputHi dflags
        location3 | Just fn <- ohi = location2{ ml_hi_file = fn }
                  | otherwise      = location2

    -- Take -o into account if present
    -- Very like -ohi, but we must *only* do this if we aren't linking
    -- (If we're linking then the -o applies to the linked thing, not to
    -- the object file for one module.)
    -- Note the nasty duplication with the same computation in compileFile above
    let expl_o_file = outputFile dflags
        location4 | Just ofile <- expl_o_file
                  , isNoLink (ghcLink dflags)
                  = location3 { ml_obj_file = ofile }
                  | otherwise = location3

    return location4

mkExtraObj :: DynFlags -> Suffix -> String -> IO FilePath
mkExtraObj dflags extn xs
 = do cFile <- newTempName dflags extn
      oFile <- newTempName dflags "o"
      writeFile cFile xs
      ccInfo <- liftIO $ getCompilerInfo dflags
      SysTools.runCc dflags
                ([Option        "-c",
                  FileOption "" cFile,
                  Option        "-o",
                  FileOption "" oFile]
                 ++ if extn /= "s"
                        then cOpts
                        else asmOpts ccInfo)
      return oFile
    where
      -- Pass a different set of options to the C compiler depending one whether
      -- we're compiling C or assembler. When compiling C, we pass the usual
      -- set of include directories and PIC flags.
      cOpts = map Option (picCCOpts dflags)
                    ++ map (FileOption "-I")
                            (includeDirs $ getPackageDetails dflags rtsUnitId)

      -- When compiling assembler code, we drop the usual C options, and if the
      -- compiler is Clang, we add an extra argument to tell Clang to ignore
      -- unused command line options. See trac #11684.
      asmOpts ccInfo =
            if any (ccInfo ==) [Clang, AppleClang, AppleClang51]
                then [Option "-Qunused-arguments"]
                else []


-- When linking a binary, we need to create a C main() function that
-- starts everything off.  This used to be compiled statically as part
-- of the RTS, but that made it hard to change the -rtsopts setting,
-- so now we generate and compile a main() stub as part of every
-- binary and pass the -rtsopts setting directly to the RTS (#5373)
--
mkExtraObjToLinkIntoBinary :: DynFlags -> IO FilePath
mkExtraObjToLinkIntoBinary dflags = do
   when (gopt Opt_NoHsMain dflags && haveRtsOptsFlags dflags) $ do
      log_action dflags dflags NoReason SevInfo noSrcSpan
          (defaultUserStyle dflags)
          (text "Warning: -rtsopts and -with-rtsopts have no effect with -no-hs-main." $$
           text "    Call hs_init_ghc() from your main() function to set these options.")

   mkExtraObj dflags "c" (showSDoc dflags main)

 where
  main
   | gopt Opt_NoHsMain dflags = Outputable.empty
   | otherwise = vcat [
      text "#include \"Rts.h\"",
      text "extern StgClosure ZCMain_main_closure;",
      text "int main(int argc, char *argv[])",
      char '{',
      text " RtsConfig __conf = defaultRtsConfig;",
      text " __conf.rts_opts_enabled = "
          <> text (show (rtsOptsEnabled dflags)) <> semi,
      text " __conf.rts_opts_suggestions = "
          <> text (if rtsOptsSuggestions dflags
                      then "true"
                      else "false") <> semi,
      case rtsOpts dflags of
         Nothing   -> Outputable.empty
         Just opts -> text "    __conf.rts_opts= " <>
                        text (show opts) <> semi,
      text " __conf.rts_hs_main = true;",
      text " return hs_main(argc,argv,&ZCMain_main_closure,__conf);",
      char '}',
      char '\n' -- final newline, to keep gcc happy
     ]

-- Write out the link info section into a new assembly file. Previously
-- this was included as inline assembly in the main.c file but this
-- is pretty fragile. gas gets upset trying to calculate relative offsets
-- that span the .note section (notably .text) when debug info is present
mkNoteObjsToLinkIntoBinary :: DynFlags -> [InstalledUnitId] -> IO [FilePath]
mkNoteObjsToLinkIntoBinary dflags dep_packages = do
   link_info <- getLinkInfo dflags dep_packages

   if (platformSupportsSavingLinkOpts (platformOS (targetPlatform dflags)))
     then fmap (:[]) $ mkExtraObj dflags "s" (showSDoc dflags (link_opts link_info))
     else return []

  where
    link_opts info = hcat [
      -- "link info" section (see Note [LinkInfo section])
      makeElfNote dflags ghcLinkInfoSectionName ghcLinkInfoNoteName 0 info,

      -- ALL generated assembly must have this section to disable
      -- executable stacks.  See also
      -- compiler/nativeGen/AsmCodeGen.hs for another instance
      -- where we need to do this.
      if platformHasGnuNonexecStack (targetPlatform dflags)
        then text ".section .note.GNU-stack,\"\",@progbits\n"
        else Outputable.empty
      ]

-- | Return the "link info" string
--
-- See Note [LinkInfo section]
getLinkInfo :: DynFlags -> [InstalledUnitId] -> IO String
getLinkInfo dflags dep_packages = do
   package_link_opts <- getPackageLinkOpts dflags dep_packages
   pkg_frameworks <- if platformUsesFrameworks (targetPlatform dflags)
                     then getPackageFrameworks dflags dep_packages
                     else return []
   let extra_ld_inputs = ldInputs dflags
   let
      link_info = (package_link_opts,
                   pkg_frameworks,
                   rtsOpts dflags,
                   rtsOptsEnabled dflags,
                   gopt Opt_NoHsMain dflags,
                   map showOpt extra_ld_inputs,
                   getOpts dflags opt_l)
   --
   return (show link_info)


{- Note [LinkInfo section]
   ~~~~~~~~~~~~~~~~~~~~~~~

The "link info" is a string representing the parameters of the link. We save
this information in the binary, and the next time we link, if nothing else has
changed, we use the link info stored in the existing binary to decide whether
to re-link or not.

The "link info" string is stored in a ELF section called ".debug-ghc-link-info"
(see ghcLinkInfoSectionName) with the SHT_NOTE type.  For some time, it used to
not follow the specified record-based format (see #11022).

-}


-----------------------------------------------------------------------------
-- Look for the /* GHC_PACKAGES ... */ comment at the top of a .hc file

getHCFilePackages :: FilePath -> IO [InstalledUnitId]
getHCFilePackages filename =
  Exception.bracket (openFile filename ReadMode) hClose $ \h -> do
    l <- hGetLine h
    case l of
      '/':'*':' ':'G':'H':'C':'_':'P':'A':'C':'K':'A':'G':'E':'S':rest ->
          return (map stringToInstalledUnitId (words rest))
      _other ->
          return []

-----------------------------------------------------------------------------
-- Static linking, of .o files

-- The list of packages passed to link is the list of packages on
-- which this program depends, as discovered by the compilation
-- manager.  It is combined with the list of packages that the user
-- specifies on the command line with -package flags.
--
-- In one-shot linking mode, we can't discover the package
-- dependencies (because we haven't actually done any compilation or
-- read any interface files), so the user must explicitly specify all
-- the packages.

{-
Note [-Xlinker -rpath vs -Wl,-rpath]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-Wl takes a comma-separated list of options which in the case of
-Wl,-rpath -Wl,some,path,with,commas parses the the path with commas
as separate options.
Buck, the build system, produces paths with commas in them.

-Xlinker doesn't have this disadvantage and as far as I can tell
it is supported by both gcc and clang. Anecdotally nvcc supports
-Xlinker, but not -Wl.
-}

linkBinary :: DynFlags -> [FilePath] -> [InstalledUnitId] -> IO ()
linkBinary = linkBinary' False

linkBinary' :: Bool -> DynFlags -> [FilePath] -> [InstalledUnitId] -> IO ()
linkBinary' staticLink dflags o_files dep_packages = do
    let platform = targetPlatform dflags
        mySettings = settings dflags
        verbFlags = getVerbFlags dflags
        output_fn = exeFileName staticLink dflags

    -- get the full list of packages to link with, by combining the
    -- explicit packages with the auto packages and all of their
    -- dependencies, and eliminating duplicates.

    full_output_fn <- if isAbsolute output_fn
                      then return output_fn
                      else do d <- getCurrentDirectory
                              return $ normalise (d </> output_fn)
    pkg_lib_paths <- getPackageLibraryPath dflags dep_packages
    let pkg_lib_path_opts = concatMap get_pkg_lib_path_opts pkg_lib_paths
        get_pkg_lib_path_opts l
         | osElfTarget (platformOS platform) &&
           dynLibLoader dflags == SystemDependent &&
           WayDyn `elem` ways dflags
            = let libpath = if gopt Opt_RelativeDynlibPaths dflags
                            then "$ORIGIN" </>
                                 (l `makeRelativeTo` full_output_fn)
                            else l
                  -- See Note [-Xlinker -rpath vs -Wl,-rpath]
                  rpath = if gopt Opt_RPath dflags
                          then ["-Xlinker", "-rpath", "-Xlinker", libpath]
                          else []
                  -- Solaris 11's linker does not support -rpath-link option. It silently
                  -- ignores it and then complains about next option which is -l<some
                  -- dir> as being a directory and not expected object file, E.g
                  -- ld: elf error: file
                  -- /tmp/ghc-src/libraries/base/dist-install/build:
                  -- elf_begin: I/O error: region read: Is a directory
                  rpathlink = if (platformOS platform) == OSSolaris2
                              then []
                              else ["-Xlinker", "-rpath-link", "-Xlinker", l]
              in ["-L" ++ l] ++ rpathlink ++ rpath
         | osMachOTarget (platformOS platform) &&
           dynLibLoader dflags == SystemDependent &&
           WayDyn `elem` ways dflags &&
           gopt Opt_RPath dflags
            = let libpath = if gopt Opt_RelativeDynlibPaths dflags
                            then "@loader_path" </>
                                 (l `makeRelativeTo` full_output_fn)
                            else l
              in ["-L" ++ l] ++ ["-Xlinker", "-rpath", "-Xlinker", libpath]
         | otherwise = ["-L" ++ l]

    let dead_strip = if osSubsectionsViaSymbols (platformOS platform)
                      then ["-Wl,-dead_strip"]
                      else []
    let lib_paths = libraryPaths dflags
    let lib_path_opts = map ("-L"++) lib_paths

    extraLinkObj <- mkExtraObjToLinkIntoBinary dflags
    noteLinkObjs <- mkNoteObjsToLinkIntoBinary dflags dep_packages

    pkg_link_opts <- do
        (package_hs_libs, extra_libs, other_flags) <- getPackageLinkOpts dflags dep_packages
        return $ if staticLink
            then package_hs_libs -- If building an executable really means making a static
                                 -- library (e.g. iOS), then we only keep the -l options for
                                 -- HS packages, because libtool doesn't accept other options.
                                 -- In the case of iOS these need to be added by hand to the
                                 -- final link in Xcode.
            else other_flags ++ dead_strip ++ package_hs_libs ++ extra_libs
                 -- -Wl,-u,<sym> contained in other_flags
                 -- needs to be put before -l<package>,
                 -- otherwise Solaris linker fails linking
                 -- a binary with unresolved symbols in RTS
                 -- which are defined in base package
                 -- the reason for this is a note in ld(1) about
                 -- '-u' option: "The placement of this option
                 -- on the command line is significant.
                 -- This option must be placed before the library
                 -- that defines the symbol."

    -- frameworks
    pkg_framework_opts <- getPkgFrameworkOpts dflags platform dep_packages
    let framework_opts = getFrameworkOpts dflags platform

        -- probably _stub.o files
    let extra_ld_inputs = ldInputs dflags

    -- Here are some libs that need to be linked at the *end* of
    -- the command line, because they contain symbols that are referred to
    -- by the RTS.  We can't therefore use the ordinary way opts for these.
    let
        debug_opts | WayDebug `elem` ways dflags = [
#if defined(HAVE_LIBBFD)
                        "-lbfd", "-liberty"
#endif
                         ]
                   | otherwise            = []

    let thread_opts
         | WayThreaded `elem` ways dflags =
            let os = platformOS (targetPlatform dflags)
            in if os `elem` [OSMinGW32, OSFreeBSD, OSOpenBSD,
                             OSNetBSD, OSHaiku, OSQNXNTO, OSiOS, OSDarwin]
               then []
               else ["-lpthread"]
         | otherwise               = []

    rc_objs <- maybeCreateManifest dflags output_fn

    let link = if staticLink
                   then SysTools.runLibtool
                   else SysTools.runLink
    link dflags (
                       map SysTools.Option verbFlags
                      ++ [ SysTools.Option "-o"
                         , SysTools.FileOption "" output_fn
                         ]
                      ++ map SysTools.Option (
                         []

                      -- See Note [No PIE eating when linking]
                      ++ (if sGccSupportsNoPie mySettings
                             then ["-no-pie"]
                             else [])

                      -- Permit the linker to auto link _symbol to _imp_symbol.
                      -- This lets us link against DLLs without needing an "import library".
                      ++ (if platformOS platform == OSMinGW32
                          then ["-Wl,--enable-auto-import"]
                          else [])

                      -- '-no_compact_unwind'
                      -- C++/Objective-C exceptions cannot use optimised
                      -- stack unwinding code. The optimised form is the
                      -- default in Xcode 4 on at least x86_64, and
                      -- without this flag we're also seeing warnings
                      -- like
                      --     ld: warning: could not create compact unwind for .LFB3: non-standard register 5 being saved in prolog
                      -- on x86.
                      ++ (if sLdSupportsCompactUnwind mySettings &&
                             not staticLink &&
                             (platformOS platform == OSDarwin || platformOS platform == OSiOS) &&
                             case platformArch platform of
                               ArchX86 -> True
                               ArchX86_64 -> True
                               ArchARM {} -> True
                               ArchARM64  -> True
                               _ -> False
                          then ["-Wl,-no_compact_unwind"]
                          else [])

                      -- '-no_pie'
                      -- iOS uses 'dynamic-no-pic', so we must pass this to ld to suppress a warning; see #7722
                      ++ (if platformOS platform == OSiOS &&
                             not staticLink
                          then ["-Wl,-no_pie"]
                          else [])

                      -- '-Wl,-read_only_relocs,suppress'
                      -- ld gives loads of warnings like:
                      --     ld: warning: text reloc in _base_GHCziArr_unsafeArray_info to _base_GHCziArr_unsafeArray_closure
                      -- when linking any program. We're not sure
                      -- whether this is something we ought to fix, but
                      -- for now this flags silences them.
                      ++ (if platformOS   platform == OSDarwin &&
                             platformArch platform == ArchX86 &&
                             not staticLink
                          then ["-Wl,-read_only_relocs,suppress"]
                          else [])

                      ++ (if sLdIsGnuLd mySettings
                          then ["-Wl,--gc-sections"]
                          else [])

                      ++ o_files
                      ++ lib_path_opts)
                      ++ extra_ld_inputs
                      ++ map SysTools.Option (
                         rc_objs
                      ++ framework_opts
                      ++ pkg_lib_path_opts
                      ++ extraLinkObj:noteLinkObjs
                      ++ pkg_link_opts
                      ++ pkg_framework_opts
                      ++ debug_opts
                      ++ thread_opts
                    ))

exeFileName :: Bool -> DynFlags -> FilePath
exeFileName staticLink dflags
  | Just s <- outputFile dflags =
      case platformOS (targetPlatform dflags) of
          OSMinGW32 -> s <?.> "exe"
          _         -> if staticLink
                         then s <?.> "a"
                         else s
  | otherwise =
      if platformOS (targetPlatform dflags) == OSMinGW32
      then "main.exe"
      else if staticLink
           then "liba.a"
           else "a.out"
 where s <?.> ext | null (takeExtension s) = s <.> ext
                  | otherwise              = s

maybeCreateManifest
   :: DynFlags
   -> FilePath                          -- filename of executable
   -> IO [FilePath]                     -- extra objects to embed, maybe
maybeCreateManifest dflags exe_filename
 | platformOS (targetPlatform dflags) == OSMinGW32 &&
   gopt Opt_GenManifest dflags
    = do let manifest_filename = exe_filename <.> "manifest"

         writeFile manifest_filename $
             "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"++
             "  <assembly xmlns=\"urn:schemas-microsoft-com:asm.v1\" manifestVersion=\"1.0\">\n"++
             "  <assemblyIdentity version=\"1.0.0.0\"\n"++
             "     processorArchitecture=\"X86\"\n"++
             "     name=\"" ++ dropExtension exe_filename ++ "\"\n"++
             "     type=\"win32\"/>\n\n"++
             "  <trustInfo xmlns=\"urn:schemas-microsoft-com:asm.v3\">\n"++
             "    <security>\n"++
             "      <requestedPrivileges>\n"++
             "        <requestedExecutionLevel level=\"asInvoker\" uiAccess=\"false\"/>\n"++
             "        </requestedPrivileges>\n"++
             "       </security>\n"++
             "  </trustInfo>\n"++
             "</assembly>\n"

         -- Windows will find the manifest file if it is named
         -- foo.exe.manifest. However, for extra robustness, and so that
         -- we can move the binary around, we can embed the manifest in
         -- the binary itself using windres:
         if not (gopt Opt_EmbedManifest dflags) then return [] else do

         rc_filename <- newTempName dflags "rc"
         rc_obj_filename <- newTempName dflags (objectSuf dflags)

         writeFile rc_filename $
             "1 24 MOVEABLE PURE " ++ show manifest_filename ++ "\n"
               -- magic numbers :-)
               -- show is a bit hackish above, but we need to escape the
               -- backslashes in the path.

         runWindres dflags $ map SysTools.Option $
               ["--input="++rc_filename,
                "--output="++rc_obj_filename,
                "--output-format=coff"]
               -- no FileOptions here: windres doesn't like seeing
               -- backslashes, apparently

         removeFile manifest_filename

         return [rc_obj_filename]
 | otherwise = return []


linkDynLibCheck :: DynFlags -> [String] -> [InstalledUnitId] -> IO ()
linkDynLibCheck dflags o_files dep_packages
 = do
    when (haveRtsOptsFlags dflags) $ do
      log_action dflags dflags NoReason SevInfo noSrcSpan
          (defaultUserStyle dflags)
          (text "Warning: -rtsopts and -with-rtsopts have no effect with -shared." $$
           text "    Call hs_init_ghc() from your main() function to set these options.")

    linkDynLib dflags o_files dep_packages

linkStaticLibCheck :: DynFlags -> [String] -> [InstalledUnitId] -> IO ()
linkStaticLibCheck dflags o_files dep_packages
 = do
    when (platformOS (targetPlatform dflags) `notElem` [OSiOS, OSDarwin]) $
      throwGhcExceptionIO (ProgramError "Static archive creation only supported on Darwin/OS X/iOS")
    linkBinary' True dflags o_files dep_packages

-- -----------------------------------------------------------------------------
-- Running CPP

doCpp :: DynFlags -> Bool -> FilePath -> FilePath -> IO ()
doCpp dflags raw input_fn output_fn = do
    let hscpp_opts = picPOpts dflags
    let cmdline_include_paths = includePaths dflags

    pkg_include_dirs <- getPackageIncludePath dflags []
    let include_paths = foldr (\ x xs -> "-I" : x : xs) []
                          (cmdline_include_paths ++ pkg_include_dirs)

    let verbFlags = getVerbFlags dflags

    let cpp_prog args | raw       = SysTools.runCpp dflags args
                      | otherwise = SysTools.runCc dflags (SysTools.Option "-E" : args)

    let target_defs =
          [ "-D" ++ HOST_OS     ++ "_BUILD_OS",
            "-D" ++ HOST_ARCH   ++ "_BUILD_ARCH",
            "-D" ++ TARGET_OS   ++ "_HOST_OS",
            "-D" ++ TARGET_ARCH ++ "_HOST_ARCH" ]
        -- remember, in code we *compile*, the HOST is the same our TARGET,
        -- and BUILD is the same as our HOST.

    let sse_defs =
          [ "-D__SSE__"      | isSseEnabled      dflags ] ++
          [ "-D__SSE2__"     | isSse2Enabled     dflags ] ++
          [ "-D__SSE4_2__"   | isSse4_2Enabled   dflags ]

    let avx_defs =
          [ "-D__AVX__"      | isAvxEnabled      dflags ] ++
          [ "-D__AVX2__"     | isAvx2Enabled     dflags ] ++
          [ "-D__AVX512CD__" | isAvx512cdEnabled dflags ] ++
          [ "-D__AVX512ER__" | isAvx512erEnabled dflags ] ++
          [ "-D__AVX512F__"  | isAvx512fEnabled  dflags ] ++
          [ "-D__AVX512PF__" | isAvx512pfEnabled dflags ]

    backend_defs <- getBackendDefs dflags

    let th_defs = [ "-D__GLASGOW_HASKELL_TH__" ]
    -- Default CPP defines in Haskell source
    ghcVersionH <- getGhcVersionPathName dflags
    let hsSourceCppOpts = [ "-include", ghcVersionH ]

    -- MIN_VERSION macros
    let uids = explicitPackages (pkgState dflags)
        pkgs = catMaybes (map (lookupPackage dflags) uids)
    mb_macro_include <-
        if not (null pkgs) && gopt Opt_VersionMacros dflags
            then do macro_stub <- newTempName dflags "h"
                    writeFile macro_stub (generatePackageVersionMacros pkgs)
                    -- Include version macros for every *exposed* package.
                    -- Without -hide-all-packages and with a package database
                    -- size of 1000 packages, it takes cpp an estimated 2
                    -- milliseconds to process this file. See Trac #10970
                    -- comment 8.
                    return [SysTools.FileOption "-include" macro_stub]
            else return []

    cpp_prog       (   map SysTools.Option verbFlags
                    ++ map SysTools.Option include_paths
                    ++ map SysTools.Option hsSourceCppOpts
                    ++ map SysTools.Option target_defs
                    ++ map SysTools.Option backend_defs
                    ++ map SysTools.Option th_defs
                    ++ map SysTools.Option hscpp_opts
                    ++ map SysTools.Option sse_defs
                    ++ map SysTools.Option avx_defs
                    ++ mb_macro_include
        -- Set the language mode to assembler-with-cpp when preprocessing. This
        -- alleviates some of the C99 macro rules relating to whitespace and the hash
        -- operator, which we tend to abuse. Clang in particular is not very happy
        -- about this.
                    ++ [ SysTools.Option     "-x"
                       , SysTools.Option     "assembler-with-cpp"
                       , SysTools.Option     input_fn
        -- We hackily use Option instead of FileOption here, so that the file
        -- name is not back-slashed on Windows.  cpp is capable of
        -- dealing with / in filenames, so it works fine.  Furthermore
        -- if we put in backslashes, cpp outputs #line directives
        -- with *double* backslashes.   And that in turn means that
        -- our error messages get double backslashes in them.
        -- In due course we should arrange that the lexer deals
        -- with these \\ escapes properly.
                       , SysTools.Option     "-o"
                       , SysTools.FileOption "" output_fn
                       ])

getBackendDefs :: DynFlags -> IO [String]
getBackendDefs dflags | hscTarget dflags == HscLlvm = do
    llvmVer <- figureLlvmVersion dflags
    return $ case llvmVer of
               Just n -> [ "-D__GLASGOW_HASKELL_LLVM__=" ++ format n ]
               _      -> []
  where
    format (major, minor)
      | minor >= 100 = error "getBackendDefs: Unsupported minor version"
      | otherwise = show $ (100 * major + minor :: Int) -- Contract is Int

getBackendDefs _ =
    return []

-- ---------------------------------------------------------------------------
-- Macros (cribbed from Cabal)

generatePackageVersionMacros :: [PackageConfig] -> String
generatePackageVersionMacros pkgs = concat
  -- Do not add any C-style comments. See Trac #3389.
  [ generateMacros "" pkgname version
  | pkg <- pkgs
  , let version = packageVersion pkg
        pkgname = map fixchar (packageNameString pkg)
  ]

fixchar :: Char -> Char
fixchar '-' = '_'
fixchar c   = c

generateMacros :: String -> String -> Version -> String
generateMacros prefix name version =
  concat
  ["#define ", prefix, "VERSION_",name," ",show (showVersion version),"\n"
  ,"#define MIN_", prefix, "VERSION_",name,"(major1,major2,minor) (\\\n"
  ,"  (major1) <  ",major1," || \\\n"
  ,"  (major1) == ",major1," && (major2) <  ",major2," || \\\n"
  ,"  (major1) == ",major1," && (major2) == ",major2," && (minor) <= ",minor,")"
  ,"\n\n"
  ]
  where
    (major1:major2:minor:_) = map show (versionBranch version ++ repeat 0)

-- ---------------------------------------------------------------------------
-- join object files into a single relocatable object file, using ld -r

joinObjectFiles :: DynFlags -> [FilePath] -> FilePath -> IO ()
joinObjectFiles dflags o_files output_fn = do
  let mySettings = settings dflags
      ldIsGnuLd = sLdIsGnuLd mySettings
      osInfo = platformOS (targetPlatform dflags)
      ld_r args cc = SysTools.runLink dflags ([
                       SysTools.Option "-nostdlib",
                       SysTools.Option "-Wl,-r"
                     ]
                        -- See Note [No PIE eating while linking] in SysTools
                     ++ (if sGccSupportsNoPie mySettings
                          then [SysTools.Option "-no-pie"]
                          else [])

                     ++ (if any (cc ==) [Clang, AppleClang, AppleClang51]
                          then []
                          else [SysTools.Option "-nodefaultlibs"])
                     ++ (if osInfo == OSFreeBSD
                          then [SysTools.Option "-L/usr/lib"]
                          else [])
                        -- gcc on sparc sets -Wl,--relax implicitly, but
                        -- -r and --relax are incompatible for ld, so
                        -- disable --relax explicitly.
                     ++ (if platformArch (targetPlatform dflags)
                                `elem` [ArchSPARC, ArchSPARC64]
                         && ldIsGnuLd
                            then [SysTools.Option "-Wl,-no-relax"]
                            else [])
                     ++ map SysTools.Option ld_build_id
                     ++ [ SysTools.Option "-o",
                          SysTools.FileOption "" output_fn ]
                     ++ args)

      -- suppress the generation of the .note.gnu.build-id section,
      -- which we don't need and sometimes causes ld to emit a
      -- warning:
      ld_build_id | sLdSupportsBuildId mySettings = ["-Wl,--build-id=none"]
                  | otherwise                     = []

  ccInfo <- getCompilerInfo dflags
  if ldIsGnuLd
     then do
          script <- newTempName dflags "ldscript"
          cwd <- getCurrentDirectory
          let o_files_abs = map (\x -> "\"" ++ (cwd </> x) ++ "\"") o_files
          writeFile script $ "INPUT(" ++ unwords o_files_abs ++ ")"
          ld_r [SysTools.FileOption "" script] ccInfo
     else if sLdSupportsFilelist mySettings
     then do
          filelist <- newTempName dflags "filelist"
          writeFile filelist $ unlines o_files
          ld_r [SysTools.Option "-Wl,-filelist",
                SysTools.FileOption "-Wl," filelist] ccInfo
     else do
          ld_r (map (SysTools.FileOption "") o_files) ccInfo

-- -----------------------------------------------------------------------------
-- Misc.

writeInterfaceOnlyMode :: DynFlags -> Bool
writeInterfaceOnlyMode dflags =
 gopt Opt_WriteInterface dflags &&
 HscNothing == hscTarget dflags

-- | What phase to run after one of the backend code generators has run
hscPostBackendPhase :: DynFlags -> HscSource -> HscTarget -> Phase
hscPostBackendPhase _ HsBootFile _    =  StopLn
hscPostBackendPhase _ HsigFile _      =  StopLn
hscPostBackendPhase dflags _ hsc_lang =
  case hsc_lang of
        HscC -> HCc
        HscAsm | gopt Opt_SplitObjs dflags -> Splitter
               | otherwise                 -> As False
        HscLlvm        -> LlvmOpt
        HscNothing     -> StopLn
        HscInterpreted -> StopLn

touchObjectFile :: DynFlags -> FilePath -> IO ()
touchObjectFile dflags path = do
  createDirectoryIfMissing True $ takeDirectory path
  SysTools.touch dflags "Touching object file" path

haveRtsOptsFlags :: DynFlags -> Bool
haveRtsOptsFlags dflags =
         isJust (rtsOpts dflags) || case rtsOptsEnabled dflags of
                                        RtsOptsSafeOnly -> False
                                        _ -> True

-- | Find out path to @ghcversion.h@ file
getGhcVersionPathName :: DynFlags -> IO FilePath
getGhcVersionPathName dflags = do
  dirs <- getPackageIncludePath dflags [toInstalledUnitId rtsUnitId]

  found <- filterM doesFileExist (map (</> "ghcversion.h") dirs)
  case found of
      []    -> throwGhcExceptionIO (InstallationError ("ghcversion.h missing"))
      (x:_) -> return x

-- Note [-fPIC for assembler]
-- When compiling .c source file GHC's driver pipeline basically
-- does the following two things:
--   1. ${CC}              -S 'PIC_CFLAGS' source.c
--   2. ${CC} -x assembler -c 'PIC_CFLAGS' source.S
--
-- Why do we need to pass 'PIC_CFLAGS' both to C compiler and assembler?
-- Because on some architectures (at least sparc32) assembler also chooses
-- the relocation type!
-- Consider the following C module:
--
--     /* pic-sample.c */
--     int v;
--     void set_v (int n) { v = n; }
--     int  get_v (void)  { return v; }
--
--     $ gcc -S -fPIC pic-sample.c
--     $ gcc -c       pic-sample.s -o pic-sample.no-pic.o # incorrect binary
--     $ gcc -c -fPIC pic-sample.s -o pic-sample.pic.o    # correct binary
--
--     $ objdump -r -d pic-sample.pic.o    > pic-sample.pic.o.od
--     $ objdump -r -d pic-sample.no-pic.o > pic-sample.no-pic.o.od
--     $ diff -u pic-sample.pic.o.od pic-sample.no-pic.o.od
--
-- Most of architectures won't show any difference in this test, but on sparc32
-- the following assembly snippet:
--
--    sethi   %hi(_GLOBAL_OFFSET_TABLE_-8), %l7
--
-- generates two kinds or relocations, only 'R_SPARC_PC22' is correct:
--
--       3c:  2f 00 00 00     sethi  %hi(0), %l7
--    -                       3c: R_SPARC_PC22        _GLOBAL_OFFSET_TABLE_-0x8
--    +                       3c: R_SPARC_HI22        _GLOBAL_OFFSET_TABLE_-0x8

{- Note [Don't normalise input filenames]

Summary
  We used to normalise input filenames when starting the unlit phase. This
  broke hpc in `--make` mode with imported literate modules (#2991).

Introduction
  1) --main
  When compiling a module with --main, GHC scans its imports to find out which
  other modules it needs to compile too. It turns out that there is a small
  difference between saying `ghc --make A.hs`, when `A` imports `B`, and
  specifying both modules on the command line with `ghc --make A.hs B.hs`. In
  the former case, the filename for B is inferred to be './B.hs' instead of
  'B.hs'.

  2) unlit
  When GHC compiles a literate haskell file, the source code first needs to go
  through unlit, which turns it into normal Haskell source code. At the start
  of the unlit phase, in `Driver.Pipeline.runPhase`, we call unlit with the
  option `-h` and the name of the original file. We used to normalise this
  filename using System.FilePath.normalise, which among other things removes
  an initial './'. unlit then uses that filename in #line directives that it
  inserts in the transformed source code.

  3) SrcSpan
  A SrcSpan represents a portion of a source code file. It has fields
  linenumber, start column, end column, and also a reference to the file it
  originated from. The SrcSpans for a literate haskell file refer to the
  filename that was passed to unlit -h.

  4) -fhpc
  At some point during compilation with -fhpc, in the function
  `deSugar.Coverage.isGoodTickSrcSpan`, we compare the filename that a
  `SrcSpan` refers to with the name of the file we are currently compiling.
  For some reason I don't yet understand, they can sometimes legitimally be
  different, and then hpc ignores that SrcSpan.

Problem
  When running `ghc --make -fhpc A.hs`, where `A.hs` imports the literate
  module `B.lhs`, `B` is inferred to be in the file `./B.lhs` (1). At the
  start of the unlit phase, the name `./B.lhs` is normalised to `B.lhs` (2).
  Therefore the SrcSpans of `B` refer to the file `B.lhs` (3), but we are
  still compiling `./B.lhs`. Hpc thinks these two filenames are different (4),
  doesn't include ticks for B, and we have unhappy customers (#2991).

Solution
  Do not normalise `input_fn` when starting the unlit phase.

Alternative solution
  Another option would be to not compare the two filenames on equality, but to
  use System.FilePath.equalFilePath. That function first normalises its
  arguments. The problem is that by the time we need to do the comparison, the
  filenames have been turned into FastStrings, probably for performance
  reasons, so System.FilePath.equalFilePath can not be used directly.

Archeology
  The call to `normalise` was added in a commit called "Fix slash
  direction on Windows with the new filePath code" (c9b6b5e8). The problem
  that commit was addressing has since been solved in a different manner, in a
  commit called "Fix the filename passed to unlit" (1eedbc6b). So the
  `normalise` is no longer necessary.
-}
