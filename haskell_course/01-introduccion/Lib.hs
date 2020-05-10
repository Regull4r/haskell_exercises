{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import Prelude hiding (head, tail, init, last, sum, length, maximum, even, concat, reverse)

{-
  Implementa y define el tipo de la función "incrementar"
-}
incrementar :: Int -> Int
incrementar x = x + 1

{-
  Implementa la función "mul" y define el tipo de la función.
-}

mul :: Int -> Int -> Int
mul x y = x * y

{-
  Implementa la función "duplicar" en base a "mul"
-}

duplicar :: Int -> Int
duplicar = mul 2


{-
Lista:
  Una lista cuyos elementos son de tipo t, podría ser una
lista vacía [] o un elemento de tipo t añadido a la cabeza
de una secuencia de elementos de tipo t. (eh:et)
-}

-- Funciones auxiliares

{-
Implementa una función que dada una lista, devuelva su primer elemento.
Si es vacía, la lista devuelve:
   error "No se puede obtener un elemento de una lista vacía"
-}
head :: [a] -> a
head []       = error "No se puede obtener un elemento de una lista vacía"
head (x : xs) = x

{-
Implementa una función que data una lista, devuelva toda la lista excepto el primer elemento.
-}
tail :: [a] -> [a]
tail []       = error "Lista vacía"
tail (x : xs) = xs

{-
Implementa una función que dada una lista devuelva toda la lista excepto el último elemento.
-}
init :: [a] -> [a]
init []       = error "Lista vacía"
init [a     ] = []
init (x : xs) = x : init xs


{-
Implementa una función que dada una lista devuelva el último elemento.
-}
last :: [a] -> a
last []       = error "Vacía"
last [x     ] = x
last (_ : xs) = last xs

{-
Implementa una función que dada un lista de elementos de tipo a, devuelva
  una lista con valor dado al final.
-}
append :: [a] -> a -> [a]
append []       a = [a]
append (x : xs) e = x : append xs e

{-
Implementa una función que dada una lista, devuelva la lista del revés.
Conseto: usa la función myAppend
-}
reverse :: [a] -> [a]
reverse []       = []
reverse (x : xs) = append (reverse xs) x

{-
Implementa una función que dadas dos litas, devuelva una sola lista que es la
  concatenación de las dos listas.
Conseto: usa myAppen
-}
concat :: [a] -> [a] -> [a]
concat [] []       = []
concat [] l        = l
concat l  []       = l
concat l  (x : xs) = concat (append l x) xs

-- MAP
{-
Implementa una función que dado un número cualquiera, calcule su cuadrado.
-}
square :: Num a => a -> a
square x = x * x

{-
Implementa una función que dada una lista de números, calcule el cuadrado de todos
esos número y los devuelva en forma de lista.
Conseto: usa la función square.
-}
squares :: Num a => [a] -> [a]
squares []       = []
squares (x : xs) = square x : squares xs

{-
Implementa una función que dada una lista de números, devuelva una lista con los
  números convertidos a string.
Conseto: usa la función show
-}
stringize :: Show a => [a] -> [String]
stringize []       = []
stringize (x : xs) = show x : stringize xs

-- FILTER

{-
  Implementa una función que devuelve True si un número dado es par, en caso contrario, False
-}
even :: Integral a => a -> Bool
even x = x `mod` 2 == 0



{-
  Implementa una función que devuelve True si un número dado es mayor que 100, en otro caso, False
-}
big :: (Num a, Ord a) => a -> Bool
big x = x > 100

{-
  Implementa una función que dada una lista de números, devuelve una lista con solo los pares.
-}
evens :: Integral a => [a] -> [a]
evens []       = []
evens (x : xs) = if even x then x : evens xs else evens xs

{-
  Implementa una función que dada una lista de números, devuelva una lista con solo los
  números mayores que 100.
-}
bigs :: (Num a, Ord a) => [a] -> [a]
bigs []       = []
bigs (x : xs) = if big x then x : bigs xs else bigs xs

-- FOLD

{-
  Implementa una función que dada una lista, devuelva la longitud de la lista.
-}
length :: [a] -> Int
length []       = 0
length (x : xs) = 1 + length xs

{-
  Implementa una función que, dada una lista de números, devuelva la suma de ellos.
-}
sum :: Num a => [a] -> a
sum []       = 0
sum (x : xs) = x + sum xs

{-
  Implementa una función que dada una lista de números, devuelva el mayor número de la lista.
-}
maximum :: (Num a, Ord a) => [a] -> a
maximum [x          ] = x
maximum (x : x' : xs) = if x > x' then maximum (x : xs) else maximum (x' : xs)

-- Slower
maximum' [x     ] = x
maximum' (x : xs) = if x > maximum xs then x else maximum xs
{-----------------------------------------------------------------------------------------------}
{-
  De la Kata https://www.codewars.com/kata/complementary-dna

  El ADN es una proteína compleja que se encuentra en el
  núcleo de las células y constituye el principal constituyente
  del material genético de los seres vivos.

  http://en.wikipedia.org/wiki/DNA
  En las cadenas de ADN, los símbolos "A" y "T" se complementan,
  al igual que "C" y "G".

  Dada una lista de caracteres (puede que vacía) devolver la complementaria.

  coadn [] -> []
  coadn ["A","T","G","C"] -> ["T","A","C","G"]
  coadn ["G","T","A","T"] -> ["C","A","T","A"]
  coadn ["A","A","A","A"] -> ["T","T","T","T"]
-}

-- ADN complementario (co-adn)
coadn :: [Char] -> [Char]
coadn [] = []
coadn (x:xs)
  | x == 'A' = 'T':coadn xs 
  | x == 'T' = 'A':coadn xs
  | x == 'C' = 'G':coadn xs
  | x == 'G' = 'C':coadn xs

{-

  Y ahora para más diversión con tipos <3
-}

data Nucleotido = A | T | G | C  deriving (Show, Eq)
type ADN = [Nucleotido]

coadn' ::  ADN -> ADN
coadn' [] = []
coadn' (x:xs)
  | x == A = T:coadn' xs
  | x == T = A:coadn' xs
  | x == C = G:coadn' xs
  | x == G = C:coadn' xs