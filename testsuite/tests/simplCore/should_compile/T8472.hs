{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}

module T8472(t) where

#if ADDR
import GHC.Exts(Ptr(..), Addr#)
import Foreign.Ptr
#else
import GHC.Exts(Int(..), Int#)
#endif

type Fun = (Int, Int -> IO ())

gve :: Int -> (Int -> IO ()) -> Fun
gve n f = (n, f . (n+))
{-# INLINE gve #-}

#if ADDR
fun :: Addr# -> Int -> IO ()
fun addr = \n -> print (Ptr addr `plusPtr` n)
#else
fun :: Int# -> Int -> IO ()
fun x = \n -> print (I# x + n)
#endif
{-# INLINE fun #-}

comb :: Fun -> Fun -> Fun
comb (n, f) (m, g) = (n + m, \x -> f x >> g x)

t :: Fun -> [Fun]
t n =
  [ n
  `comb`
#if ADDR
  gve 3 (fun "foo"#)
  `comb`
  gve 3 (fun "bar"#)
#else
  gve 3 (fun 9897114#)
  `comb`
  gve 3 (fun 9897122#)
#endif
  ]
