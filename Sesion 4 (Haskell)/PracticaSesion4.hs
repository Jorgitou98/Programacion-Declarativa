-- Jorge Villarrubia
-- Ejercicio 1

data Nat = Cero | Suc Nat deriving (Eq, Ord)

infixl 6 +++
(+++):: Nat -> Nat -> Nat

Cero +++ x = x
Suc x +++ y = Suc(x +++ y)

infixl 7 ***
(***):: Nat -> Nat -> Nat
Suc(Cero) *** x = x
Suc x *** y = y +++ x *** y

natToInt:: Nat -> Int

natToInt Cero = 0
natToInt (Suc x) = 1 + natToInt x

instance Show Nat where
 show x = show(natToInt x)
 
-- Ejericio 2

data Complejo = C (Float, Float) deriving Eq

infix 6 ++++
(++++):: Complejo -> Complejo -> Complejo

C (r1,i1) ++++ C (r2,i2) = C (r1+r2,i1+i2)

infix 7 ****
(****):: Complejo -> Complejo -> Complejo

C(r1,i1) **** C(r2,i2) = C (r1*r2-i1*i2, r1*i2 + r2* i1)

infix 6 -+-+
(-+-+):: Complejo -> Complejo -> Complejo

C (r1,i1) -+-+ C (r2,i2) = C (r1-r2,i1-i2)

instance Num Complejo where
 (+) = (++++)
 (*) = (****)
 (-) = (-+-+)
 
instance Show Complejo where
 show (C (r, i))
  | i > 0 = show (r) ++ " + " ++ show (i) ++ "i"
  | i < 0 = show (r) ++ " - " ++ show (negate(i)) ++ "i"
  | True = show (r)
  
-- Ejercicio 3

class Medible a where
 tamanyo:: a ->Int

instance Medible Bool where
 tamanyo x = 1

instance Medible [a] where
 tamanyo x = length x
 
instance Medible (a,b) where
 tamanyo x = 2

-- Ejercicio 4

type Punto = (Int, Int)

data Direccion = UP | DOWN | RIGHT | LEFT deriving (Eq, Ord, Show)

destino:: Punto -> [Direccion] -> Punto

destino p dir = foldl f p dir
 where f (c1,c2) UP = (c1,c2+1)
       f (c1,c2) DOWN = (c1,c2-1)
       f (c1,c2) RIGHT = (c1+1,c2)
       f (c1,c2) _ = (c1-1,c2)

trayectoria:: Punto -> [Direccion] -> [Punto]

trayectoria p [] = [p]

trayectoria (c1,c2) (x:xs)
 | x == UP = (c1,c2+1):(trayectoria (c1,c2+1) xs)
 | x == DOWN = (c1,c2-1):(trayectoria (c1,c2-1) xs) 
 | x == RIGHT = (c1+1,c2):(trayectoria (c1+1,c2) xs)
 | True = (c1-1,c2):(trayectoria (c1-1,c2) xs)
 

-- Ejercicio 5

data Arbol a = Hoja a | Nodo a [Arbol a] deriving Show

listaHojas :: Arbol a -> [a]

listaHojas (Hoja x) = [x]
listaHojas (Nodo x xs) = foldl f [] xs
 where f x y = x ++ listaHojas y

listaNodos :: Arbol a -> [a]

listaNodos (Hoja x) = [x]
listaNodos (Nodo x xs) = foldl f [x] xs
 where f x y = x ++ listaNodos y

repMax :: Ord a => Arbol a -> Arbol a

repMax (Hoja x) = Hoja x
repMax (Nodo x xs) = Nodo (max x m2) (foldl f [] xs)
 where m2 = maximum (listaNodos (Nodo x xs))
       f xs (Hoja _) = xs ++ [Hoja (max x m2)]  
       f xs (Nodo z ys) = xs ++ [repMax (Nodo (max x m2) ys)]
        