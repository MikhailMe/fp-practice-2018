module Task2_2 where

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

-- foldl f s [a1, a2, a3, a4] = (((s `f` a1) `f` a2) `f` a3) `f` a4
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f x []     = x
foldl f x (y:ys) = foldl f (f x y) ys

-- foldr f s [a1, a2, a3, a4] = a1 `f` (a2 `f` (a3 `f` (a4 `f` s)))
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f x []     = x
foldr f x (y:ys) = f y (foldr f x ys)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f = maybe [] (\ (a, b) -> a : unfoldr f b) . f

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product lst = foldl (*) 1 lst

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\ item lst -> case item of 
                                      Just item -> item:lst
                                      Nothing -> lst) []

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal = 	todo

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = foldr (\ x xs -> if f x then x : xs else xs) []

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem x = foldl (\ s x' -> x' == x || s) False

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step 
 | from > to = error "error: from > to" 
 | otherwise = unfoldr (\ from -> if (from + 1) > to then Nothing else Just(from, from + step)) from

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append first second = foldr (:) second first 

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = unfoldr (\ lst -> Just(lst, drop n lst)) (take n lst)

-- unfoldr f = maybe [] (\ (a, b) -> a : unfoldr f b) . f

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative n"

-- *Task2_2> take n a
-- [1,2,3,4,5]
-- *Task2_2> drop n a
-- [6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30]
