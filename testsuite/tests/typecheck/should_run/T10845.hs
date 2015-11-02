{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -dcore-lint #-}

import GHC.Exception
import GHC.Stack

f1 :: (?loc :: CallStack) => CallStack
f1 = let y = (?loc :: CallStack)
     in y

f2 :: (?loc :: CallStack) => CallStack
f2 = let y x = ?loc
     in y 0

main :: IO ()
main = do putStrLn $ showCallStack f1
          putStrLn $ showCallStack f2
