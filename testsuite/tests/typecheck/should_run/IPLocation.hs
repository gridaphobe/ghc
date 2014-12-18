{-# LANGUAGE ImplicitParams, RankNTypes #-}

module Main where

import GHC.Location

f0 = putStrLn $ showLocation ?loc
     -- should just show the location of ?loc

f1 :: (?loc :: Location) => IO ()
f1 = putStrLn $ showLocation ?loc
     -- should show the location of ?loc *and* f1's call-site

f2 :: (?loc :: Location) => IO ()
f2 = do putStrLn $ showLocation ?loc
        putStrLn $ showLocation ?loc
     -- each ?loc should refer to a different location, but they should
     -- share f2's call-site

f3 :: ((?loc :: Location) => () -> IO ()) -> IO ()
f3 x = x ()
       -- the call-site for the functional argument should be added to the
       -- stack..

f4 :: (?loc :: Location) => ((?loc :: Location) => () -> IO ()) -> IO ()
f4 x = x ()
       -- as should the call-site for f4 itself

f5 :: (?loc1 :: Location) => ((?loc2 :: Location) => () -> IO ()) -> IO ()
f5 x = x ()
       -- furthermore, we ignore the *name* of the IP,
       -- and only care about the type

main = do f0
          f1
          f2
          f3 (\ () -> putStrLn $ showLocation ?loc)
          f4 (\ () -> putStrLn $ showLocation ?loc)
          f5 (\ () -> putStrLn $ showLocation ?loc3)
