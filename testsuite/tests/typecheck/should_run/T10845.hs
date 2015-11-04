{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -dcore-lint #-}

import GHC.Exception
import GHC.Stack

f1 :: (?loc :: CallStack) => CallStack
-- we can infer a CallStack for let-binders
f1 = let y x = (?loc :: CallStack)
     in y 0

f2 :: (?loc :: CallStack) => CallStack
-- but only when we would infer an IP.
-- i.e. the monomorphism restriction prevents us
-- from inferring a CallStack.
f2 = let y = (?loc :: CallStack)
     in y

main :: IO ()
main = do putStrLn $ showCallStack f1
          putStrLn $ showCallStack f2
