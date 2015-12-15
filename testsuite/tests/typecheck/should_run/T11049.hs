{-# LANGUAGE ImplicitParams #-}
import GHC.Stack.Types

foo :: (?callStack :: CallStack) => [Int]
foo = map (srcLocStartLine . snd) (getCallStack ?callStack)

bar1 :: [Int]
bar1 = foo

bar2 :: [Int]
bar2 = let ?callStack = freezeCallStack ?callStack in foo

main :: IO ()
main = do
  print bar1
  print bar2
