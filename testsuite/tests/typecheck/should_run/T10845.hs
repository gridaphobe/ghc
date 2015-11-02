{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -dcore-lint #-}

import GHC.Stack

f :: (?loc :: CallStack) => Int
f = let y = getCallStack ?loc
    in length y

main :: IO ()
main = print f
