{-# LANGUAGE ImplicitParams, RankNTypes #-}
{-# OPTIONS_GHC -dcore-lint #-}
module Main where

import GHC.Exception
import GHC.Types

f0 = putStrLn $ showCallStack ?loc
     -- should be empty

f1 :: (?loc :: CallStack) => IO ()
f1 = putStrLn $ showCallStack ?loc
     -- should show the location of f1's call-site

f3 :: ((?loc :: CallStack) => () -> IO ()) -> IO ()
f3 x = x ()
       -- the call-site for the functional argument should be added to the
       -- stack..

f4 :: (?loc :: CallStack) => ((?loc :: CallStack) => () -> IO ()) -> IO ()
f4 x = x ()
       -- as should the call-site for f4 itself

f5 :: (?loc1 :: CallStack) => ((?loc2 :: CallStack) => () -> IO ()) -> IO ()
f5 x = x ()
       -- we only push new call-sites onto CallStacks with the name IP name

f6 :: (?loc :: CallStack) => Int -> IO ()
f6 0 = putStrLn $ showCallStack ?loc
f6 n = f6 (n-1)
       -- recursive functions add a SrcLoc for each recursive call

main = do f0
          f1
          f3 (\ () -> putStrLn $ showCallStack ?loc)
          f4 (\ () -> putStrLn $ showCallStack ?loc)
          f5 (\ () -> putStrLn $ showCallStack ?loc3)
          f6 5
