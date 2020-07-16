-- Jorge Villarrubia Elvira

-- Ejercicio 1

-- Apartado a
añosDecimal:: Fractional a => a
añosDecimal = (10^10)/(365*24*60*60)
-- Apartado b
-- Hacemos un let para utilizar variables locales
datosEntero:: Integral a => (a, a, a, a, a)
datosEntero = let seg = 10^10 in
	let minutosEnteros = div seg 60 in 
	let horasEnteros = div minutosEnteros 60 in 
	let diasEnteros = div horasEnteros 24 in 
	let añosEnteros = div diasEnteros 365 in 
	(añosEnteros, diasEnteros - añosEnteros * 365, horasEnteros - diasEnteros *24, minutosEnteros - horasEnteros * 60, seg - minutosEnteros * 60 )

-- Apartado c
f:: (Fractional a) => a -> a
f seg = seg/(365*24*60*60)

g:: (Integral a) => a -> (a,a,a,a,a)
g seg = let minutosEnteros = div seg 60 in 
	let horasEnteros = div minutosEnteros 60 in 
	let diasEnteros = div horasEnteros 24 in 
	let añosEnteros = div diasEnteros 365 in 
	(añosEnteros, diasEnteros - añosEnteros * 365, horasEnteros - diasEnteros *24, minutosEnteros - horasEnteros * 60, seg - minutosEnteros * 60 )

-- Ejercicio 2


-- last [1..10^5] Regular
-- last [1..10^7] Regular
-- last [1..10^20] Mucho
-- head [1..10^20] Poco
-- last [10^20..1] Excepcion. Toma la lista como vacía.
-- head (tail [1..10^20]) Poco
-- length [1..10^20] Mucho
-- last (take (10^7) [1..10^20]) Regular
-- head (take (10^7) ([1..100] ++ [1..10^20])) Poco
-- last (take 100 ([1..10^20] ++ [1..100])) Regular
-- last (drop 100 ([1..10^20] ++ [1..100])) Mucho
-- head (drop (10^7) ([1..10^20] ++ [1..100])) Poco
-- [1..10^7]==[1..10^7] Poco
-- [1..10^20]==[1..10^20] Mucho
-- [1..10^20]==[1..10^20+1] Mucho
-- [1..10^20]==[2..10^20] Poco, evaluacion perezosa
-- head (reverse [1..10^7]) Regular
-- last (reverse [1..10^7]) Regular
-- reverse [1..10^20] == reverse [1..10^20+1] Mucho


-- Ejercicio 3
media:: Fractional a => [a] -> a
-- media l = (sum l) / (length l) da error de tipos
media (x:xs) = (sum (x:xs)) / (fromIntegral(length (x:xs)))
media [] = error "Es necesario algun elemento en la lista"
-- Ejercicio 4

-- Con guardas
digitos:: Integral a => a -> a
digitos x 
	| (abs(x) <= 9) = 1
	| otherwise = digitos (div x 10) + 1
-- Con guardas
reduccion:: Integral a => a -> a
reduccion x = let {suma y
					| (abs(y) <= 9) = abs(y)
					| otherwise = suma (div y 10) + (abs (mod y 10));
					z = abs(x)
					}
	in if z <= 9 then z else reduccion (suma z)
-- Con guardas
perm:: Integral a => a -> a
perm n
	| (n == 0) = 1
	| (n >= 1) = n * (perm (n-1))
	| otherwise = error "factorial de un negativo"

var:: Integral a => a -> a -> a
var n m = div (perm n)(perm(n-m))

comb :: Integral a => a -> a -> a
comb n m = div (perm n)(perm(n-m) * perm(m))

-- Ejercicio 5

-- Estricto en el primer argumento, no en el segundo
(&&&):: Bool -> Bool -> Bool
(&&&) False _ = False
(&&&) _ False = False
(&&&) True True = True

-- Estricto en el segundo argumento, no en el primero
y :: Bool -> Bool -> Bool
y _ False = False
y False _ = False
y True True = True

-- Estricto en primer argumento solo.
and :: Bool -> Bool -> Bool
and True x = x
and False _ = False

-- Estricto en ambos. Se puede hacer con muchos menos casos
ylog :: Bool -> Bool -> Bool
ylog True True = True
ylog True False = False
ylog False True = False
ylog False False = False


