module Task5_1 where

import Todo(todo)

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) = 
    let rec = DCons left h (list2dlist' rec t)
    in rec

index :: DList a -> Int -> a
index DNil ind                       = error "error: list is empty"
index (DCons left current right) 0   = current
index (DCons left current right) ind = index right (ind - 1)

insertAt :: DList a -> Int -> a -> DList a
-- если лист пустой, то всегда вставляем в нулевой элемент
insertAt DNil _ value = DCons DNil value DNil
insertAt (DCons left lvalue right) index value
    -- дошли до элемента и в следующий надо вставить
    -- создаем элемент
    -- прокидываем себя же в правый элемент 
    | index == 1 = let rec = DCons left lvalue (insertAt' rec value right) in rec
    -- случай когда вставляем в первый элемент
    | index == 0 = let rec = DCons DNil value (insertAt' rec lvalue right) in rec
    -- если вставляем в индекс, который больше длины списка, то ошибка, иначе идём дальше
    | index /= 1 = case right of 
        DNil -> error "error: index more than list length"
        _    -> DCons left lvalue (insertAt right (index - 1) value)

insertAt' :: DList a -> a -> DList a -> DList a
-- обновляем последний элемент
insertAt' left value DNil = DCons left value DNil
-- обновляем все правые элементы, прокидывая "себя" в правый элемент
insertAt' left value (DCons left' value' right) =
    let rec = DCons left value (insertAt' rec value' right) in rec
    
removeAt :: DList a -> Int -> DList a
removeAt DNil index = error "error: list is empty"
removeAt list index = todo
