module T10506 where

-- we should have a tick for the occurrence of `+`
plus x y = x + y

-- also in a section
plus1 = (+1)

-- we should also have a tick for the occurrence of `plus`
plus2 = plus 2

-- and in an infix application
three = 1 `plus` 2

-- what about binders, should we have ticks for the binding sites of x,y,z?
foo = let x = 1 in case Just (x + y) of
                     Just z -> z + 5
  where Just y = Just 4
