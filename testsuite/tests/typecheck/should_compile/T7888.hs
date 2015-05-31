{-# LANGUAGE RankNTypes, MagicHash #-}

module T7888 where
import GHC.Base( undefined )
import GHC.Prim

f :: (forall a. a) -> b
f = undefined

g :: Int -> Int#
g _ = undefined
