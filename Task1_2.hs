module Task1_2 where

import Todo(todo)
import Data.Fixed
import Prelude hiding (sin, cos, gcd)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = do
	let newX = mod' x (2 * pi) :: Double
	let n = newX :: Double
	let s = 0.0 :: Double
	let i = 1 :: Int
	calculateSin n newX s i

calculateSin :: Double -> Double -> Double -> Int -> Double
calculateSin n x s i = do
	let eps = 1e-8 :: Double
	if (abs n < eps) then s else
		calculateSin (n * (generalNum x / denomForSin i)) x (s + n) (i + 1)

generalNum :: Double -> Double
generalNum x = (-1.0) * x * x

denomForSin :: Int -> Double
denomForSin i = ((2 * (fromIntegral i)) * (2 * (fromIntegral i) + 1))

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = do
	let newX = mod' x (2 * pi) :: Double
	let n = 1.0 :: Double
	let s = 0.0 :: Double
	let i = 1 :: Int
	calculateCos n newX s i

calculateCos :: Double -> Double -> Double -> Int -> Double
calculateCos n x s i = do
	let eps = 1e-8 :: Double
	if (abs n < eps) then s else
		calculateCos (n * (generalNum x / denomForCos i)) x (s + n) (i + 1)

denomForCos :: Int -> Double
denomForCos i = ((2 * (fromIntegral i) - 1) * (2 * (fromIntegral i)))

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x y 
	| x < 0     = error "error: argument less zero"
 	| y < 0     = error "error: argument less zero"
 	| x == 0    = y
 	| y == 0	= x
 	| otherwise = gcd y (mod x y)

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to 
	| from == to   	= False
	| mod' x 1 == 0 = True 
	| otherwise     = doesSquareBetweenExist (from + 1) to
	where x = sqrt (fromIntegral from)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year 
	| day < 0 = False
	| month < 0 = False
	| year < 0 = False
	| otherwise = check day month (year `mod` 4 == 0)

check :: Integer -> Integer -> Bool -> Bool
check day month isLeapYear =
		case month of
		2 -> if isLeapYear then checkDays day 30 else checkDays day 29
		x | elem x [1, 3, 5, 7, 8, 10, 12] -> checkDays day 32
		x | elem x [4, 6, 9, 11] -> checkDays day 31
		otherwise -> False

checkDays :: Integer -> Integer -> Bool
checkDays days limit = if days < limit then True else False


-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y 
	| y < 0     = error "error: degree less zero"
	| y == 0    = 1
	| y == 1    = x
	| otherwise = x * pow x (y - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x 
	| x < 0     = error "error: argument less zero"
	| x == 0    = True
	| x == 1    = True
	| otherwise = divHelp x 2

divHelp :: Integer -> Integer -> Bool
divHelp x divider
	| x == divider       = True
	| mod x divider == 0 = False
	| otherwise          = divHelp x (divider + 1)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть  
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
