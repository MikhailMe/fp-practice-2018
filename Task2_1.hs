module Task2_1 where

import Todo(todo)

import Prelude hiding (lookup)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = EmptyTree |
 Node Integer v (TreeMap v) (TreeMap v)
    deriving (Show, Eq)

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTree

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains EmptyTree k = False
contains (Node key value left right) k
    | k == key = True
    | k < key  = contains left k
    | k > key  = contains right k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup k EmptyTree = error "error: key not found"
lookup k (Node key value left right)
    | k == key = value
    | k < key  = lookup k left
    | k > key  = lookup k right

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) EmptyTree = Node k v EmptyTree EmptyTree
insert (k, v) (Node key value left right)
    | k < key   = Node key value (insert (k, v) left) right
    | k > key   = Node key value left (insert (k, v) right)
    | otherwise = (Node key value left right)

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove i EmptyTree = error "error: key not found"
remove i (Node key value left right)
    | i < key  = Node key value (remove i left) right
    | i > key  = Node key value left (remove i right)
    | i == key = case (left, right) of
                      (EmptyTree, EmptyTree) -> EmptyTree
                      (left, EmptyTree)      -> left
                      (EmptyTree, right)     -> right
                      (left, right)          -> helperRemove left right

helperRemove :: TreeMap v -> TreeMap v -> TreeMap v
helperRemove left' EmptyTree = left'
helperRemove left' (Node key value EmptyTree right) = Node key value left' right
helperRemove left' (Node key value left right)      = Node key value (helperRemove left' left) right

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i EmptyTree = error "error: key not found"
nearestLE i (Node key value left right)
    | i == key = (key, value)
    | i < key  = nearestLE i left
    | i > key  = case (right) of 
        Node key' value' left' _ | i == key' -> (key', value')
                                 | i > key'  -> nearestLE i right
                                 | i < key'  -> case (left') of 
                                        EmptyTree -> (key, value)
                                        otherwise -> nearestLE i left'
        otherwise -> (key, value) 

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert EmptyTree lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree EmptyTree = []
listFromTree t         = inorder t

inorder :: TreeMap v -> [(Integer, v)]
inorder (Node key value left right) = listFromTree left ++ [(key, value)] ++ listFromTree right

preorder :: TreeMap v -> [(Integer, v)]
preorder (Node key value left right) = [(key, value)] ++ listFromTree left ++ listFromTree right

postorder :: TreeMap v -> [(Integer, v)]
postorder (Node key value left right) = listFromTree left ++ listFromTree right ++ [(key, value)]

-- Поиск k-той порядковой статистики дерева 
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i EmptyTree = error "error: key not found"
kMean i (Node key value left right)
    | i == (size left) = (key, value)
    | i < (size left)  = kMean i left
    | i > (size left)  = kMean (i - size left - 1) right

size :: TreeMap v -> Integer
size EmptyTree             = 0
size (Node _ _ left right) = size left + 1 + size right