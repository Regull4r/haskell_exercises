{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import Prelude hiding (map, filter, foldr)

{- Implementa la funcion map sobre listas -}
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

{- Implementa la funcion filter sobre listas -}
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs) = if f x
                    then x : filter f xs
                    else filter f xs

{- Implementa la funcion foldr sobre listas -}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ a [] = a
foldr f acc (x:xs) = f x (foldr f acc xs)

foldll :: (b -> a -> b) -> b -> [a] -> b
foldll _ a [] = a
foldll f acc (x:xs) = foldl f (f acc x) xs

{- Implementa las funciones map y filter usando foldr -}
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) [] 

{- Implementa la funcion filter sobre listas -}
filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x xs -> if f x then x:xs else xs) []

{- implementa las funciones del modulo 1 usando unicamente funciones de orden superior -}
-- MAP

{-
Implementa una función que dada una lista de números, calcule el cuadrado de todos
esos número y los devuelva en forma de lista.
Conseto: usa la función square.
-}
squares :: Num a => [a] -> [a]
squares = map (^2) 

{-
Implementa una función que dada una lista de números, devuelva una lista con los
  números convertidos a string.
Conseto: usa la función show
-}
stringize :: Show a => [a] -> [String]
stringize = map show

-- FILTER

{-
  Implementa una función que dada una lista de números, devuelve una lista con solo los pares.
-}
evens :: Integral a => [a] -> [a]
evens = filter even

{-
  Implementa una función que dada una lista de números, devuelva una lista con solo los
  números mayores que 100.
-}
bigs :: (Num a, Ord a) => [a] -> [a]
bigs = filter (>100)

-- FOLD

{-
  Implementa una función que dada una lista, devuelva la longitud de la lista.
-}
length :: [a] -> Int
length = foldr (\_ acc -> acc + 1) 0

{-
  Implementa una función que, dada una lista de números, devuelva la suma de ellos.
-}
sum :: Num a => [a] -> a
sum = foldr (+) 0

{-
  Implementa una función que dada una lista de números, devuelva el mayor número de la lista.
-}
maximum :: (Num a, Ord a) => [a] -> a
maximum l = foldr (\x acc -> if acc >= x then acc else x) (head l) l

{-----------------------------------------------------------------------------------------------}
{- Utilizando el arbol binario definido a continuacion -}
data BinaryTree a = Leaf
     | Node (BinaryTree a) a (BinaryTree a) deriving (Show)

{- Implementa el mapTree -}
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node l c r) = Node (mapTree f l) (f c) (mapTree f r)

{- Implementa la funcion foldTree-}
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node l c r) = f c (foldTree f (foldTree f acc r) l)