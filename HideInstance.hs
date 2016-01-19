module HideInstance where

import GhcPlugins
import TcPluginM

import Class
import PrelNames
import TcRnTypes

import Data.Maybe

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = mkHideInstance }

mkHideInstance :: [CommandLineOption] -> Maybe TcPlugin
mkHideInstance _ = Just hideInstance

hideInstance :: TcPlugin
hideInstance = TcPlugin
  { tcPluginInit = return ()
  , tcPluginSolve = const hideInstanceSolver
  , tcPluginStop = const $ return ()
  }

hideInstanceSolver :: [Ct] -- ^ Given constraints
                   -> [Ct] -- ^ Derived constraints
                   -> [Ct] -- ^ Wanted constraints
                   -> TcPluginM TcPluginResult
hideInstanceSolver _given _derived wanted = do

  tcPluginTrace "hide-instance" (ppr wanted)

  functor <- lookup gHC_BASE "Functor"

  let bad = mapMaybe (isBad functor) wanted

  tcPluginTrace "hide-instance" (ppr bad)

  if null bad
    then return $ TcPluginOk [] []
    else return $ TcPluginContradiction bad

  where

  lookup md s = tcLookupClass =<< lookupOrig md (mkTcOcc s)

  isBad :: Class -> Ct -> Maybe Ct
  isBad f ct
    | Just (cls, [a]) <- splitTyConApp_maybe (ctPred ct)
    , cls == classTyCon f
    , Just (tc, _) <- splitTyConApp_maybe a
    , isTupleTyCon tc
    = Just ct -- (CDictCan (cc_ev ct) f [a] False)

    | otherwise
    = Nothing
