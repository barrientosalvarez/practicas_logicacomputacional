{-Practica 1: Recordando Haskell
Primera parte: Numeros Binarios Positivos

Realizado por: 
Barrientos Alvarez Jorge Miguel Aaron  |  jma.barrientos@ciencias.unam.mx
-}

{-
"Esta gramatica representa numeros binarios iguales o mayores a 1. Es importante
notar que como estamos construyendo a BinPos tenemos al bit menos significativo
como constructor
    *U: repressenta el dígito 1
    *Cero x: representa el binario x0 donde x es un binario
    *Uno x: representa el biario x1 donde x es un binario"-}

data BinPos = U | Cero(BinPos) | Uno(BinPos)

{-Instancia de la clase Show que nos permite representar nuestros numeros Binarios
  Positivos en el formato estandar: con el bit más significativo a la izquierda y
  con el bit menos significativo a la derecha y usando los simbolos '0' y '1'; 
  esto a diferencia de como deben de escribirse al llamar a una función y a diferencia
  de como son representados internmente los numeros BinPos, ya que estos tienen
  el bit menos significativo a la izquierda y el más significativo a la derecha 
  (el cual recordemos siempre es 'U'), y siempre deben estar en el fotmato 
  Cero(x) o Uno(y) donde y y x son BinPos-}
instance Show(BinPos) where
  show(Cero a)=show(a)++"0"
  show(Uno a)=show(a)++"1"
  show(U)="1"

{-FUNCION SUCESOR: dado un BinPos regresa a su sucesor-}
sucesor::BinPos->BinPos
sucesor U= Cero U
sucesor (Cero x)=Uno x
sucesor (Uno x)= Cero (sucesor x)

{-FUNCION SUMA: dados dos BinPos regresa su suma. La estrategia que se siguió 
  para esta función fue ir considerando uno por uno los siguientes casos de la 
  suma en numeros binarios: 0+0=0, 1+0=1, 0+1=1 y 1+1=0 acarreando 1; entre otros
  casos base-}
suma::BinPos->BinPos->BinPos
suma U U = Cero U
suma x U=sucesor(x)
suma U x=sucesor(x)
suma (Cero x) (Cero y)=Cero(suma(x) (y))
suma (Cero x) (Uno y)=Uno(suma(x)(y))
suma (Uno x) (Cero y)=Uno(suma(x)(y))
suma (Uno x) (Uno y)=Cero(suma(suma(x)(y))(U))

{-FUNCION RESTA: dados dos BinPos regresa la resta del primero menos el segundo.
  En caso de que el segundo numero sea mayor al primero, se regresará U.
  Esta función se hizo un poco distinta a como nos sugirió el profesor. En vez
  de ir caso por caso considerando la forma de los BinPos por su bit menos 
  significativo, y viendo qué sucede cuando, en binario, restas 1-1, 0-1, etc, 
  lo que hicimos fue pensar en lo siguiente: si tienes dos números enteros m y n,
  y haces la resta m-n, podemos pensar que a m le estamos restando 1 n veces.
  Para hacer esto (restar 1 n veces) usamos la función antecesor-}
resta::BinPos->BinPos->BinPos
resta U U= U
resta U x = U
resta x U=antecesor(x)
resta x y=resta(antecesor(x))(antecesor(y))


{-FUNCION ANTECESOR: dado un BinPos regresa su antecesor. Esta será una funcion
   auxiliar para la funcion resta-}
antecesor::BinPos->BinPos
antecesor U=U
antecesor (Cero U)=U
antecesor (Uno x)=Cero x
antecesor (Cero x)=Uno(antecesor x)

{-FUNCION PRODUCTO: dados dos BinPos regresa su producto. Tomamos en cuenta los 
   siguientes casos de la multiplicacion de numeros binarios: 0x0=0, 1x0=0, 
   0x1=0, 1x1=0. Nos ayudamos de la la función suma, ya que, si hacemos una 
   multiplicacion (pensando en cómo nos enseñaron en la primaria), eventualmente
   tenemos que sumar.-}
producto::BinPos->BinPos->BinPos
producto U U = U
producto x U = x
producto U x = x
producto (Uno x) (Uno y)=suma(Uno x)(Cero(producto(Uno x)(y)))
producto (Cero x) (Cero y)=Cero(Cero(producto(x)(y)))
producto (Uno x) (Cero y)=Cero(producto(Uno x)(y))
producto (Cero x) (Uno y)=suma(Cero x)(Cero(producto(Cero x)(y)))

{-FUNCION BINPOST TO INT: dado un BinPos regresa su representacion en un numero
  natural representado bajo el tipo Int. Para pasar de un numero binario a su 
  representación decimal tenemos que hacer lo siguiente: al igual que
  en los numeros decimales, los dígitos de los números binarios toman su valor
  de acuerdo a la posición que ocupan. Este valor es el resultado de multiplicar
  1 o 0 por 2^n, donde n es la posición de dicho dígito (comenzando en 0 y de 
  izquierda a la derecha). Por lo tanto, lo que tenemos que hacer para pasar un 
  número de binario a decimal es multiplicar los dígitos por su potencia de dos 
  correnpondiente y sumar los resultados de dichos productos.-}
binPosToInt::BinPos->Int
binPosToInt U = 1
binPosToInt (Uno x)=1+2*binPosToInt(x)
binPosToInt (Cero x)=2*binPosToInt(x)


{-FUNCION INT TO BINPOS: dado un numero natural de tipo Int regresa 
   su representacion bajo el tipo BinPos. Para pasar de un numero decinal a su
   representacion en binario la idea es similar a la de pasar un numeros binario 
   a decimal, solo que, esta vez, hay que ir dividiendo entre dos e ir 
   colocando el residuo (de derecha a izquierda) de dichas divisiones.-}
intToBinPos::Int->BinPos
intToBinPos 1 = U
intToBinPos 0 = U
intToBinPos n
    | mod n 2 == 0 = Cero (intToBinPos(div n 2))
    | otherwise    = Uno (intToBinPos(div n 2))



----------------------------------PRUEBAS--------------------------------------

--PRUEBAS FUNCION SUCESOR 
suc1=sucesor(Cero(Cero(Uno U))) 
--regresa: el BinPos Uno(Cero(Uno U)), pero en la representacion '1101' 
suc2=sucesor(Uno(Uno U))
--regresa: el BinPOs Cero(Cero(Cero U)), pero en la representacion '1000'


--PRUEBAS FUNCION SUMA
sum1=suma(Cero U)(Uno U)
--regresa: el BinPos Uno(Cero U), pero en la representacion '101'
sum2=suma(Uno(Uno U))(Cero(Uno(Cero U)))
--regresa: el BinPos Uno(Cero(Cero(Cero U))), pero en la representacion '10001'

--PRUEBAS FUNCION RESTA 
res1=resta(Cero(Uno(Cero U)))(Cero(Cero(Uno U)))
--regresa: el BinPos U, pero en la representacion '1', ya que el segundo numero es mayor al primero
res2=resta(Cero(Cero(Uno U)))(Cero(Uno(Cero(U))))
--regresa: el BinPos (Cero U), pero en la representacion '10'


--PRUEBAS FUNCION ANTECESOR
ant1=antecesor(Cero(Uno(Uno U)))
--regresa: el BinPos Uno(Cero(Uno U)), pero en la representacion '1101'
ant2=antecesor(Uno(Uno(Cero(Cero U))))
--regresa: el BinPos Cero(Uno(Cero(Cero U))), pero en la representacion '10010'


--PRUEBAS FUNCION PRODUCTO
pro1=producto(Uno(Uno(Cero U)))(Uno U)
--regresa: el BinPos Uno(Cero(Cero(Cero(Cero U)))), pero en la representación '100001'
pro2=producto(Cero(Uno U))(Cero U)
--regres: el BinPos Cero(Cero(Uno U)), pero en la representación '1100'


--PRUEBAS FUNCION BinPosToInt 
bpti1=binPosToInt(Uno(Cero(Cero U))) 
--regresa: el número 9
bpti2=binPosToInt(Uno(Cero(Uno(Uno(Cero(Cero(Cero U)))))))
--regresa: el número 141


--PRUEBAS FUNCION IntToBinPos
itbp1=intToBinPos(127)
--regresa: el BinPos Uno(Uno(Uno(Uno(Uno(Uno U))))), pero en la representacion '1111111'
itbp2=intToBinPos(42)
--regresa: el BinPos Cero(Uno(Cero(Uno(Cero U)))), pero en la representacion '101010'
