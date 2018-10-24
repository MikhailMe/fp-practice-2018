module Task2_2 where

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f x []     = x
foldl f x (y:ys) = foldl f (f x y) ys

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f x []     = x
foldr f x (y:ys) = f y (foldr f x ys)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f x = case f x of
                   Just (y , g) -> y:(unfoldr f g)
                   Nothing      -> []

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
catMaybes = todo

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal = todo

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = foldr (\ x xs -> if f x then x : xs else xs) []

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem x = foldl (\ s x' -> x' == x || s) False

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = reverse (rangeToHelper from to step [])
-- переписать
rangeToHelper :: Integer -> Integer -> Integer -> [Integer] -> [Integer]
rangeToHelper from to step lst
              | (from + 1) > to = lst
              | otherwise = rangeToHelper (from + step) to step (from : lst)

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append first second = foldr (:) second first 

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = todo -- groupsHelp lst n 


combine :: [a] -> [a] -> [[a]]
combine x y = [x, y]

push :: [a] -> [[a]] -> [[a]]
push = (:)