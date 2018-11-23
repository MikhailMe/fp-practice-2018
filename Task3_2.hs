module Task3_2 where

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList RNil               = []
rlistToList (RCons next value) = value : (rlistToList next)

listToRList :: [a] -> ReverseList a
listToRList []    = RNil
listToRList (h:t) = RCons (listToRList t) h

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance (Eq a) => Eq (ReverseList a) where
    (==) RNil RNil                                 = True
    (==) RNil _                                    = False
    (==) _ RNil                                    = False
    (==) (RCons next1 value1) (RCons next2 value2) = next1 == next2 && value2 == value2

instance (Ord a) => Ord (ReverseList a) where
    (<=) RNil RNil                                 = True
    (<=) RNil _                                    = True
    (<=) _ RNil                                    = False
    (<=) (RCons next1 value1) (RCons next2 value2) = value1 <= value2 || next1 <= next2

instance (Show a) => Show (ReverseList a) where
    show RNil               = "[]"
    show (RCons RNil value) = show value
    show (RCons next value) = show next ++ "," ++ show value
  
instance Monoid (ReverseList a) where
    mempty = RNil

    mappend RNil list               = list
    mappend list RNil               = list
    mappend list (RCons next value) = RCons (mappend list next) value

    mconcat = foldr mappend mempty

instance Functor ReverseList where
    fmap _ RNil                      = RNil
    fmap function (RCons next value) = RCons (fmap function next) (function value)