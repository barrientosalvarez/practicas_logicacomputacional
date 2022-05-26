{-
Práctica 4: unificación.

Integrantes del equipo:

Almanza Torres José Luis | jose-luis@ciencias.unam.mx

Barrientos Alvarez Jorge Miguel Aaron | jma.barrientos@ciencias.unam.mx

Velázquez Caballero Ixchel | ix.ve.ca@ciencias.unam.mx
-}



module Unificacion where

import Data.List

-- Representa las cadenas.
type Nombre = String

-- Un término es una variable o un símbolo de función seguido de una
-- lista de términos.
data Termino = V Nombre
             | T Nombre [Termino]
             deriving Eq

-- Instancia show para terminos.
instance Show Termino where
  show (V nombre)    = nombre
  show (T nombre []) = nombre
  show (T nombre ts) = nombre ++ concat [show ts]

-- Una variable es un termino.
type Variable = Termino

-- Una sustitución es un par formado por una variable y un término.
type Sustitucion = [(Variable, Termino)]

-- 1.Función que nos dice si un un termino es una variable.
esVariable :: Termino -> Bool
esVariable (V j) = True
esVariable (T r []) = False
esVariable (T r (l:ls))=False

-- 2.Función que dado un término, regresa su lista de variables.
variables :: Termino -> [Variable]
variables (V j) = [V j]
variables (T r [])=[]
variables (T r (l:ls))=variablesEnLista(l:ls) 

-- 3.Función que regresa la lista de variables dada una lista de términos.
variablesEnLista :: [Termino] -> [Variable]
variablesEnLista [] = []
variablesEnLista [V j]=[V j]
variablesEnLista (l:j:ls)=variables((!!0)(l:j:ls)) `union` variablesEnLista(j:ls)
variablesEnLista [T r []]=[]
variablesEnLista [T r [l]]=variables(l)
variablesEnLista [T r (l:j:ls)]=variables((!!0) (l:j:ls)) `union` variablesEnLista(j:ls)

-- 4.Función que representa a la sustitución identidad.
epsilon :: Sustitucion
epsilon = []
-- :)

-- 5.Función que dada una sustitución, obtenemos su dominio.
dominio :: Sustitucion -> [Variable]
dominio l = map fst l 

-- 6.Función que dada una sustitución y una variable, regresa la
-- aplicación de la sustitución a la variable.
aplicaVar :: Sustitucion -> Variable -> Termino
aplicaVar [] v = v
aplicaVar (l:ls) v=if(v==fst(l)) then snd(l)
                                 else aplicaVar(ls) (v)

-- 7.Función que dada una sustitución y un término, regresa la
-- aplicación de la sustitución al término.
aplicaT :: Sustitucion -> Termino -> Termino
aplicaT [] r = r
aplicaT (l:ls) (V j)=aplicaVar(l:ls)(V j)
aplicaT (l:ls) (T r(v:vs))= T r([aplicaT(l:ls)(j) | j<-(v:vs)])


-- 8.Función que regresa la sustitución obtenida, eliminando los pares
-- cuyos elementos son iguales.
reduce :: Sustitucion -> Sustitucion
reduce [] = []
reduce (l:ls)=[(a,b) | (a,b)<-(l:ls), a/=b]

-- 9.Función que dadas dos sustituciones, regresa su composición.
composicion :: Sustitucion -> Sustitucion -> Sustitucion
composicion (l:ls) (v:vs)=(reduce [(r, aplicaT(v:vs)(j)) | (r,j) <- (l:ls) ])
                                ++[(d, e) | (d, e)<- (v:vs), d `notElem` (dominio (l:ls)) ]

-- 10.Función que dados dos términos, regresa la lista formada por el
-- unificador más general de ambos términos. Si no son unificables,
-- regresa la lista vacía.
unifica :: Termino -> Termino -> [Sustitucion]
unifica (V x) (V y)
    | x==y = [epsilon]
    | otherwise = [[(V x,V y)]]
unifica (V x) t2 =
    [ [(V x,t2)] | (V x) `notElem` variables t2 ]
unifica t1 (V y) =
    [ [(V y,t1)] | (V y) `notElem` variables t1 ]
unifica (T f ts) (T g rs) =
    [ u | f==g, u <- unificaListas ts rs ]

-- 11.Función que regresa la lista formada por el unificador más general
-- de las listas de términos.
unificaListas :: [Termino] -> [Termino] -> [Sustitucion]
unificaListas [] [] = [epsilon]
unificaListas [] (r:rs) = []
unificaListas (t:ts) [] = []
unificaListas (t:ts) (r:rs) =
      [ composicion s7 s6
          | s6 <- unifica t r,
          s7 <- unificaListas [aplicaT s6 t | t <- ts]
                                  [aplicaT s6 r | r <- rs] ]
---------------------------------------------------------------------------------
--------                             EJEMPLOS                            --------
---------------------------------------------------------------------------------

-- Ejemplos de variables.

x = V "x"
y = V "y"
z = V "z"
u = V "u"
w = V "w"

-- Ejemplos de constantes.

a = T "a" []
b = T "b" []
c = T "c" []

-- Ejemplos de simbolos de función.

f = T "f"
g = T "g"
h = T "h"
p = T "p"

-- Ejemplos de sustituciones
s1 = [(x, a), (z, f [x, y])]
s2 = [(x, z), (y, u)]
s3 = [(z, x), (x, b), (u, c)]
s4 = [(u, f [x]), (y, a)]
s5 = [(x, h [z]), (y, g [b])]

-- Ejemplos de las notas

ejemplo1 = unificaListas [w1] [w2]
w1 = f [w, f [x, h [z]]]
w2 = f [g [x], f [x, y]]

ejemplo2 = unificaListas [y1] [y2]
y1 = p [x, f [y]]
y2 = p [g [y, a], f [b]]

-- ejemplos funciones

esVariable1 = esVariable x
-- Regresa: True

esVariable2 = esVariable a
-- Regresa: False

variables1 = variables (g [f [x,y], z])
-- Regresa: [x,y,z]

variablesEnLista1 = variablesEnLista [f [x,y], g [f [x, y], z]]
-- Regresa: [x,y,z]

dominio1 = dominio s1
-- Regresa: [x,z]

aplicaVar1 = aplicaVar s1 x
-- Regresa: a

aplicaVar2 = aplicaVar s1 y
-- Regresa: y

aplicaVar3 = aplicaVar s1 z
-- Regresa: f [x, y]

aplicaT1 = aplicaT s1 (g [f [x, y], z])
-- Regresa: g [f [a, y], f [x, y]]

reduce1 = reduce [(x,a), (y,y), (z, f [x,y])]
-- Regresa: [(x,a), (z, f [x,y])]

composicion1 = composicion s2 s3
-- Retresa: [(y, c), (z, x), (u, c)]

composicion2 = composicion s4 s5
-- Retresa: [(u, f [h [z]]), (y, a), (x, h [z])]

unifica1 = unifica a a
-- Regresa: [[]]

unifica2 = unifica x a
-- Regresa: [[(x, a)]]

unifica3 = unifica x (f[y])
-- Regresa: [[(x, f[y])]]

unifica4 = unifica x (f[x])
-- Regresa: []

unifica5 = unifica (f[y]) x
-- Regresa: [[(x, f[y])]]

unifica6 = unifica (f[x]) x
-- Regresa: []

unificaListas1 = unificaListas [x, f[x], y] [a, y, z]
-- Regresa: [[(z, f[a]), (y, f[a]), (x, a)]]

unificaListas2 = unificaListas [x, f[x]] [y, y]
-- Regresa: []
