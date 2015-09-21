module T10844 where

import T10844a

n :: Int
n = 0
{-# NOINLINE n #-}

main = print (foo n)
