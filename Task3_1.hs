module Task3_1 where

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

instance Show WeirdPeanoNumber where
    show Zero        = "Zero"
    show (Succ succ) = "Succ, " ++ show succ
    show (Pred pred) = "Pred, " ++ show pred

myToInteger :: WeirdPeanoNumber -> Integer
myToInteger wpn = helperInt wpn 0

helperInt :: WeirdPeanoNumber -> Integer -> Integer
helperInt Zero amount        = amount
helperInt (Succ succ) amount = helperInt succ (amount + 1)
helperInt (Pred pred) amount = helperInt pred (amount - 1)

instance Eq WeirdPeanoNumber where
    (==) wpn1 wpn2 = myToInteger wpn1 == myToInteger wpn2

instance Ord WeirdPeanoNumber where
    (<=) wpn1 wpn2 = (myToInteger wpn1) <= (myToInteger wpn2)

normalize :: WeirdPeanoNumber -> WeirdPeanoNumber
normalize wpn = myFromInteger (myToInteger wpn) Zero

myFromInteger :: Integer -> WeirdPeanoNumber -> WeirdPeanoNumber
myFromInteger 0 wpn = wpn
myFromInteger n wpn = myFromInteger (n - 1) (Succ wpn)

{-plus :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
plus wpn1 Zero = wpn1
plus wpn1 (Succ wpn2) = Succ(wpn1 + wpn2)
plus wpn1 (Pred wpn2) = Pred(wpn1 + wpn2)
-}
--instance Num WeirdPeanoNumber where
     



  --   fromInteger n = myFromInteger n Zero 

-- instance Enum WeirdPeanoNumber where

-- instance Real WeirdPeanoNumber where

-- instance Integral WeirdPeanoNumber where
