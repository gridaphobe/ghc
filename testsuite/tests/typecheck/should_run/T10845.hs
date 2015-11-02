{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -dcore-lint #-}

import GHC.Exception
import GHC.Stack

f1 :: (?loc :: CallStack) => CallStack
f1 = let y = (?loc :: CallStack)
     in y

f2 :: (?loc :: CallStack) => CallStack
f2 = let y x = (?loc :: CallStack)
     in y 0

f3 :: (?loc :: CallStack) => CallStack
-- in this case, `y :: (?loc :: t) => a -> t`, so we don't get an entry
-- corresponding to the occurrence of `?loc`. ugh.. i don't think
-- there's anything we can do about this, but on the other hand, it
-- seems *very* unlikely to occur in the real world.
f3 = let y x = ?loc
     in y 0

main :: IO ()
main = do putStrLn $ showCallStack f1
          putStrLn $ showCallStack f2
          putStrLn $ showCallStack f3
