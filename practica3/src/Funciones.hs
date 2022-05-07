{-
- Barrientos Alvarez Jorge Miguel Aaron  |  jma.barrientos@ciencias.unam.mx
-}


module Funciones where

import Data.Char

----------------------------------------------------------------------------------
-------------------------------myMap y my Filter----------------------------------
----------------------------------------------------------------------------------
-- Implementación funcion 'map'
myMap::(a -> b)->[a]->[b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs


-- Implementación función 'filter'
myFilter::(a -> Bool)->[a]->[a]
myFilter _ []=[]
myFilter f l=[y | y<-l, f(y) == True]


{- Funcion que recibe una lista de listas, le suma un elemento a cada elemento de 
    las listas y el resultado lo multiplica por 3.14. Se usó la implementación de
    myMap y funciones lambda. -}
sumaYMultiplica:: Fractional a =>[[a]]->a->[[a]]
sumaYMultiplica l e = myMap(myMap(\x-> x*3.14)) (myMap(myMap(\x->x+e)) l)


{- Funcion que recibe una lista de cadenas (o lista de listas de caracteres) y a 
    cada una de estas le concatena "pro" al inicio y "azo" al final. Se usó la 
    implementación de myMap-}
concatenaProazo::[[Char]]->[[Char]]
concatenaProazo l = myMap (\x->"pro"++x) (myMap(\x->x++"azo") l)


{- Funcion que recibe una lista de listas y filtra aquellas cuya longitud sea mayor
    a 4 pero menor a 8. Se usó la implementación de myFilter, así como la función 
    auxiliar lon. -}
mayorMenor::[[a]]->[[a]]
mayorMenor l=myFilter(\x->lon x <8)(myFilter(\x-> lon x > 4) l)


{- Funcion que recibe una cadena (o lista de caracteres) y filtra aquellos que son
    letras pero que no son vocales. Se usó la implementación de myFilter y la 
    función isAlpha de la biblioteca Data.Char.-}
filtrarVocales::[Char]->[Char]
filtrarVocales l = myFilter(\x -> isAlpha x)(myFilter(\x->not(x `elem` "aeiouAEIOU")) l)



----------------------------------------------------------------------------------
------------------------------FUNCIONES AUXILIARES--------------------------------
----------------------------------------------------------------------------------
lon::[a]->Int
lon []=0
lon (x:xs)=1+lon(xs)


----------------------------------------------------------------------------------
------------------------------------PRUEBAS---------------------------------------
----------------------------------------------------------------------------------

-- Lista punto 1
list1=[[1],[1,2],[1,2,3],[1,2,3,4]]


-- Lista punto 2
list2=["firmala","gio","firmala","firmala"]


-- Lista punto 3
list3=[[1,2,3,4],[1,2],[1,2,3,4,5,6,7,8],[1,2,3,4,5]]


-- Lista punto 4
list4=["looks like a school bus for 6 yo pimps"]
