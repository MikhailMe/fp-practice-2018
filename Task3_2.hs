module Task3_2 where

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList = todo

listToRList :: [a] -> ReverseList a
listToRList = todo

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance (Show a) => Show (ReverseList a) where
    show RNil = "[]"

instance (Eq a) => Eq (ReverseList a) where
    (==) rl1 rl2 = todo

instance (Ord a) => Ord (ReverseList a) where
    (<=) rl1 rl2 = todo

instance Semigroup (ReverseList a) where
    (<>) = todo

instance Monoid (ReverseList a) where
    mempty = todo

instance Functor ReverseList where
    fmap = todo