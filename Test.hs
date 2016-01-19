module Test where

-- test :: (Bool, Bool)
-- test = fmap not (True,False)

test = fmap not (F True True)

data F a b = F a b
