{-# LANGUAGE PatternSynonyms #-}
module ShouldCompile where

pattern Single :: () => (Show a) => a -> [a]
pattern Single x = [x]

-- f :: (Show a) => [a] -> a
foobar (Single x) = x
