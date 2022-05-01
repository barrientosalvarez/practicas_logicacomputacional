{-
- Lógica Computacional 2022-2
- Profesor: Francisco Hernández Quiroz
- Practica 2: Lógica proposicional.
- Hecho por: Barrientos Alvarez Jorge Miguel Aaron  |  jma.barrientos@ciencias.unam.mx
-}

module LogProp where

import Data.List

data Prop =
  Var Name
  | Neg Prop
  | Conj Prop Prop
  | Disy Prop Prop
  | Impl Prop Prop
  | Syss Prop Prop

-- Variables.
type Name = String

-- Sustitución.
type Sust = [(Name, Name)]

-- Estado de una variable.
type Estado = (Name, Bool)

-- Solamente muchos estados.
type Estados = [Estado]

-- Un renglon es una lista de variables con su asignacion (True/False)
-- y un resultado.
type Renglon = (Estados, Bool)

-- Una tabla es solo una lista de renglones.
type Tabla = [Renglon]

-- Instancia para Show
instance Show Prop where
  show (Var x)    = show x
  show (Neg x)    = "¬"++ (show x)
  show (Conj x y) = "(" ++ show x ++ " Λ " ++ show y ++ ")"
  show (Disy x y) = "(" ++ show x ++ " ∨ " ++ show y ++ ")"
  show (Impl x y) = "(" ++ show x ++ " → " ++ show y ++ ")"
  show (Syss x y) = "(" ++ show x ++ " ↔ " ++ show y ++ ")"

-- Instancia para Eq
instance Eq Prop where
  (Var x)    == (Var y)      = x == y
  (Neg x)    == (Neg y)      = x == y
  (Conj x y) == (Conj x' y') = (x == x') && (y == y')
  (Disy x y) == (Disy x' y') = (x == x') && (y == y')
  (Impl x y) == (Impl x' y') = (x == x') && (y == y')
  (Syss x y) == (Syss x' y') = (x == x') && (y == y')
  _ == _ = False

--------------------------------------------------------------------------------
--------                            FUNCIONES                           --------
--------------------------------------------------------------------------------

-- | Funcion que saca todos los átomos de una proposicion.
varList :: Prop -> [Name]
varList (Var x)=[x]
varList (Neg x)=varList(x)
varList (Conj x y)=varList(x) `union` varList(y)
varList (Disy x y)=varList(x) `union` varList(y)
varList (Impl x y)=varList(x) `union` varList(y)
varList (Syss x y)=varList(x) `union` varList(y)

-- | Funcion que elimina las implicaciones y dobles implicaciones de
-- una proposicion.
equivalencia :: Prop -> Prop
equivalencia (Var x)=(Var x)
equivalencia (Neg(Var x))=(Neg(Var x))
equivalencia (Neg x)=(Neg x)
equivalencia (Conj x y)=Conj(equivalencia(x))(equivalencia(y))
equivalencia (Disy x y)=Disy(equivalencia(x))(equivalencia(y))
equivalencia (Impl x y)=Disy(equivalencia(negacion(x)))(equivalencia(y))
equivalencia (Syss x y)=Conj(equivalencia(Impl(x)(y)))(equivalencia(Impl(y)(x)))


-- | Funcion que niega una proposicion
negacion :: Prop -> Prop
negacion (Var x)=Neg (Var x)
negacion (Neg (Var x))=(Var x)
negacion (Neg(x))=x
negacion (Conj x y)=Disy(negacion(x))(negacion(y))
negacion (Disy x y)=Conj(negacion(x))(negacion(y))
negacion (Impl x y)=Conj(x)(negacion(y))
negacion (Syss x y)=negacion(Conj(Impl(x)(y))(Impl(y)(x)))

-- | Funcion que dada una proposicion y una sustitucion, sustituye las
-- variables que correspondan.
sustituye :: Prop -> Sust -> Prop
sustituye (Var p) []=(Var p)
sustituye (Var p) ((x,y):xs)=if(p==x) then (Var y)
                                      else sustituye (Var p) xs
sustituye (Neg p) xs = Neg(sustituye p xs)
sustituye (Conj p q) xs = Conj(sustituye p xs)(sustituye q xs)
sustituye (Disy p q) xs = Disy(sustituye p xs)(sustituye q xs)
sustituye (Impl p q) xs = Impl(sustituye p xs)(sustituye q xs)
sustituye (Syss p q) xs = Syss(sustituye p xs)(sustituye q xs)

-- | Funcion que dada una proposición y estados, evalua la proposicion
-- asignando el estado que corresponda. Si no existe una variable,
-- maneja el error.
interp :: Prop -> Estados -> Bool
interp (Var p) xs = valor p xs
interp (Neg p) xs = neg(interp p xs)
interp (Conj p q) xs = conj(interp p xs)(interp q xs)
interp (Disy p q) xs = disy(interp p xs)(interp q xs)
interp (Impl p q) xs = impl(interp p xs)(interp q xs)
interp (Syss p q) xs = syss(interp p xs)(interp q xs)

-- | Funcion que dada una proposicion, dice True si es tautologia,
-- False en otro caso.
esTautologia :: Prop -> Bool
esTautologia p = (modelos p)==(estados p)


-- | Funcion que dada una proposicion, dice True si es una
-- contradiccion, False en otro caso.
esContradiccion :: Prop -> Bool
esContradiccion p = neg(True `elem` ([interp p x | x<-estados(p)]))

-- | Funcion que dada una proposicion, dice True si es satisfacible,
-- False en otro caso.
esSatisfacible :: Prop -> Bool
esSatisfacible p =  True `elem` ([interp p x | x <- estados(p)])

-- | Funcion que dada una proposicion, devuelve su tabla de verdad.
tablaDeVerdad :: Prop -> Tabla
tablaDeVerdad p = [(edos, interp p edos) | edos <- mods]
  where 
    mods = estados p

-- | Funcion que devuelve la lista de todos los modelos posibles para
-- una proposición.
modelos :: Prop -> [Estados]
modelos p = [ x | x <- estados(p), (interp p x)==True]

--------------------------------------------------------------------------------
--------                           AUXILIARES                           --------
--------------------------------------------------------------------------------

{-FUNCION AUXILIAR POTENCIA: dada una lista de elementos, regresa el conjunto potencia de 
   la lista, es decir, todos los posibles subconjuntos de los elementos de la lista
   (incluyendo el vacío: [])-}
potencia :: Eq a => [a] -> [[a]]
potencia [] =[[]]
potencia (x:xs) = map(x:) pt `union` pt
  where
    pt = potencia xs

{-FUNCION AUXILIAR ESTADOS: dada una proposición regresa todos los posibles estados
   de verdad de la misma (contemplando todas las posibilidades)-}
estados :: Prop -> [Estados]
estados p = map sort (zipWith (++) true' false')
  where
    true = potencia(varList(p))
    true' = map (\x -> (map (\y -> (y, True)) x)) true
    false = reverse(potencia(varList(p)))
    false' = map (\x -> (map (\y -> (y, False)) x)) false


------------------------AUXILIARES PARA INTERPRETACION----------------------------
{-FUNCION AUXILIAR VALOR: dada una variable 'Name', y una lista de 'Estado', esta
   funcion devuelve el valor de verdad de la variable que viene indicado en la 
   lista de 'Estado'. Si la variable buscada no existe en la lista 'Estado', un 
   error sucede.-}
valor :: Name->Estados->Bool
valor a []= error "variable no incluida"
valor a ((x,b):xs)= if (a==x) then b
                              else valor a xs

{-FUNCION AUXILIAR NEG: implementación de una función ya existente. Recibe un
   valor de verdad y regresa el mismo valor pero negado. A pesar de que ya existe
   ésta función, se implementó explícitamente para darle completitud a las funciones
   auxiliares necesarias para la funcion 'interpretación'-}
neg :: Bool->Bool
neg True = False
neg False = True

{-FUNCION AUXILIAR CONJ: recibe dos valores de verdad y regresa el valor de verdad 
   de la conjunción de los mismos. Esta función funciona basándose en la 
   tabla de verdad de la conjunción de dos variables:

    P Q | P ^ Q
    ----|------
    T T |   T
    T F |   F
    F T |   F
    F F |   F
   -}
conj :: Bool->Bool->Bool
conj True True=True
conj False _ = False
conj _ False = False

{-FUNCION AUXILIAR DISY: recibe dos valores de verdad y regresa el valor de verdad
   de la disyunción de los mismos. Estaa función funciona basándose en la tabla
   de verdad de la disyunción de dos variables:

    P Q | P v Q
    ----|------
    T T |   T
    T F |   T
    F T |   T
    F F |   F
   -}
disy :: Bool->Bool->Bool
disy True _ = True
disy _ True = True
disy False False = False

{-FUNCION AUXILIAR IMPL: recibe dos valores de verdad y regresa el valor de verdad
   de la implicación del primero hacia el segundo. Esta función funciona basándose
   en la tabla de verdad de la implicación de dos variables:
   
   P Q | P => Q
   ----|-------
   T T |   T
   T F |   F
   F T |   T
   F F |   T
   -}
impl :: Bool->Bool->Bool
impl False _ = True
impl True True = True
impl True False = False

{-FUNCION AUXILIAR SYSS: recibe dos valores de verdad y regresa el valor de verdad
   de la doble implicación entre éstos. Esta función funciona basándose en la tabla
   de verdad de la doble implicación:
   
   P Q | P <=> Q
   ----|--------
   T T |    T
   T F |    F
   F T |    F
   F F |    T
   -}
syss :: Bool->Bool->Bool
syss True True = True
syss False False = True
syss True False = False
syss False True = False

--------------------------------------------------------------------------------
--------                             EJEMPLOS                           --------
--------------------------------------------------------------------------------

imp :: Prop
imp = (Impl (Var "P") (Conj (Var "Q") (Neg (Var "R"))))

------------------------------EJEMPLOS VARLIST--------------------------------
varList1 = varList imp
-- Regresa: ["P", "Q", "R"]
varList2 = varList(Disy(Impl(Var "A")(Neg(Var "B")))(Var "C"))
-- Regresa: ["A", "B", "C"]


--------------------------EJEMPLOS EQUIVALENCIA----------------------------------
equivalencia1 = equivalencia imp
-- Regresa: Disy (Neg (Var "P")) (Conj (Var "Q") (Neg (Var "R")))

equivalencia2 = equivalencia (Syss imp imp)
-- Regresa: Conj (Disy (Neg (Disy (Neg (Var "P")) (Conj (Var "Q") (Neg
-- (Var "R"))))) (Disy (Neg (Var "P")) (Conj (Var "Q") (Neg (Var
-- "R"))))) (Disy (Neg (Disy (Neg (Var "P")) (Conj (Var "Q") (Neg (Var
-- "R"))))) (Disy (Neg (Var "P")) (Conj (Var "Q") (Neg (Var "R")))))


-----------------------------EJEMPLOS NEGACIÓN---------------------------------
negacion1 = negacion imp
-- Regresa: Conj (Var "P") (Disy (Neg (Var "Q")) (Var "R"))

negacion2 = negacion (Neg imp)
-- Regresa: Impl (Var "P") (Conj (Var "Q") (Neg (Var "R")))


-----------------------------EJEMPLOS SUSTITUYE-------------------------------------
sustituye1 = sustituye imp [("P", "A"), ("Q", "B"), ("R", "C")]
-- Regresa: Impl (Var "A") (Conj (Var "B") (Neg (Var "C")))
sustituye2= sustituye (Disy(Impl(Var "A")(Neg(Var "B")))(Var "C")) [("A", "P"), ("B", "R")]
--Regresa: Disy(Impl(Var "P")(Neg(Var "R")))(Var "C")


---------------------------EJEMPLOS INTERPRETACION--------------------------------------
interp1 = interp imp [("Q",True), ("P",False)]
-- Regresa: True
interp2 = interp imp [("Q",True), ("P",True)]
-- Regresa: error
interp3 = interp imp [("Q",True), ("P",True), ("R",True)]
-- Regresa: False


-----------------------------EJEMPLOS TAUTOLOGÍA----------------------------------
tautologia1 = esTautologia(imp)
-- Regresa: False
tautologia2= esTautologia(Syss(Syss(Var "P")(Var "Q"))(Conj(Impl(Var "P")(Var "Q"))(Impl(Var "Q")(Var "P"))))
-- Regresa: True


-------------------------------EJEMPLOS CONTRADICCIÓN--------------------------------
contradiccion1 = esContradiccion(imp)
--Regresa: False
contradiccion2 = esContradiccion(Syss(Disy(Var "P")(Var "Q"))(Neg(Disy(Var "P")(Var "Q"))))
--Regresa: True


-------------------------------EJEMPLOS SATISFACIBLE--------------------------------------
satisfacible1 = esSatisfacible(imp)
--Regresa: True
satisfacible2 = esSatisfacible(Syss(Disy(Var "P")(Var "Q"))(Neg(Disy(Var "P")(Var "Q"))))
--Regresa: False


-----------------------------EJEMPLOS TABLA DE VERDAD-------------------------------------
tabla1 = tablaDeVerdad(imp)
-- Regresa: [([("P",True),("Q",True),("R",True)],False),([("P",True),("Q",True),("R",False)],True),([("P",True),("Q",False),("R",True)],False),
--([("P",True),("Q",False),("R",False)],False),([("P",False),("Q",True),("R",True)],True),([("P",False),("Q",True),("R",False)],True),
--([("P",False),("Q",False),("R",True)],True),([("P",False),("Q",False),("R",False)],True)]

tabla2 = tablaDeVerdad(Conj(Impl(Var "P")(Var "Q"))(Syss(Var "R")(Var "P")))
-- Regresa: [([("P",True),("Q",True),("R",True)],True),([("P",True),("Q",True),("R",False)],False),([("P",True),("Q",False),("R",True)],False),
--([("P",True),("Q",False),("R",False)],False),([("P",False),("Q",True),("R",True)],False),([("P",False),("Q",True),("R",False)],True),
--([("P",False),("Q",False),("R",True)],False),([("P",False),("Q",False),("R",False)],True)]


-------------------------------EJEMPLOS MODELOS-------------------------------------------
modelos1=modelos(imp)
--Regresa: [[("P",True),("Q",True),("R",False)],[("P",False),("Q",True),("R",True)],[("P",False),("Q",True),("R",False)],
--[("P",False),("Q",False), ("R",True)],[("P",False),("Q",False),("R",False)]]

modelos2=modelos(Conj(Impl(Var "P")(Var "Q"))(Syss(Var "R")(Var "P")))
--Regresa: [[("P",True),("Q",True),("R",True)],[("P",False),("Q",True),("R",False)],[("P",False),("Q",False),("R",False)]]

