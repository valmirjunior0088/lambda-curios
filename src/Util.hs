module Util
  ( (!?)
  , unique
  , subset
  , (<==>)
  , (.&&.)
  , (.||.)
  )
  where

(!?) :: Eq a => a -> [(a, b)] -> b
(!?) target = \case
  [] -> errorWithoutStackTrace "(!?): nonexistent key"
  (key, value) : _ | key == target -> value
  _ : values -> target !? values

infixl 1 !?

unique :: Eq a => [a] -> Bool
unique = \case
  [] -> True
  a : rest -> not (elem a rest) && unique rest

subset :: Eq a => [a] -> [a] -> Bool
subset ones others = all (`elem` ones) others

(<==>) :: Eq a => [a] -> [a] -> Bool
(<==>) ones others = subset ones others && subset others ones

infixl 1 <==>

(.&&.) :: Monad m => m Bool -> m Bool -> m Bool
(.&&.) one other = one >>= \case
  True -> other
  False -> return False

infixl 1 .&&.

(.||.) :: Monad m => m Bool -> m Bool -> m Bool
(.||.) one other = one >>= \case
  True -> return True
  False -> other

infixl 1 .||.
