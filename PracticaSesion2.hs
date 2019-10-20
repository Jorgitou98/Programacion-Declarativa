-- Jorge Villarrubia Elvira

-- Ejercicio 1
cuadrados:: (Num a, Eq a) => a -> [a]

cuadrados 0 = [0]
cuadrados n = cuadrados (n-1) ++ [n^2]

-------------------------------------------------

cuadradosPares:: (Num a, Eq a) => a -> [(a,a)]

cuadradosPares 0 = [(0,0)]
cuadradosPares n = cuadradosPares (n-1) ++ [(n,n^2)]

-------------------------------------------------

suma:: (Floating a, Eq a) => a

suma = sumatorio 100
 where sumatorio 1 = abs(sin(1))
       sumatorio n = sumatorio (n-1) + n * abs(sin(n))
   

-------------------------------------------------

potencias:: Integral a => a -> a

potencias n = potenciasAux 0 n
 where potenciasAux x n
        | (3^x) > n = 0
        | (mod (3^x) 100 == 67) = 1 + potenciasAux (x+1) n
        | otherwise = potenciasAux (x+1) n

-------------------------------------------------


multiplos:: Integral a => a

multiplos = multiplosMen 999
 where multiplosMen 1 = 0
       multiplosMen n = if (mod n 3 == 0 || mod n 5 == 0)
                        then n + multiplosMen (n-1)
                        else multiplosMen (n-1)

-------------------------------------------------
-- Ejercicio 2

filter2:: [a] -> (a -> Bool) -> (a -> Bool)-> ([a], [a])

filter2 xs p q = (filter p xs, filter q xs)

-------------------------------------------------

filters:: [a] -> [(a -> Bool)] -> [[a]]

filters xs ps = map (\x -> filter x xs) ps

-------------------------------------------------

mapx:: a -> [(a -> b)] -> [b]

mapx x fs = map (\y -> y x) fs

-------------------------------------------------

iguales:: Eq a => (Int -> a) -> (Int -> a) -> Int -> Int -> Bool

iguales f g m n = all (\x -> f x == g x) (take (n+1-m) (iterate (+1) m))

-------------------------------------------------

cuantos :: (a -> Bool) -> [a] -> Int

cuantos p xs  = length (filter p xs)

-------------------------------------------------

menorA:: Int -> Int -> (Int -> Bool) -> Int

-- Si no hay ninguno dara un fallo de ejecución

menorA n m p = head ( filter p (take (m+1-n)( iterate (+1) n)))

-------------------------------------------------

mayor:: Int -> (Int -> Bool) -> Int

mayor n p = head (filter p (iterate (+(-1)) n))

-------------------------------------------------

ex:: Int -> Int -> (Int -> Bool) -> Bool

ex n m p = any p (take (m+1-n)( iterate (+1) n))

-------------------------------------------------

-- Ejercicio 3

lastF:: [a] -> a

lastF (x:xs) = foldl (\x y-> y) x (x:xs)

-------------------------------------------------

reverseF:: [a] -> [a]

reverseF xs = foldl (\x y -> y:x) [] xs

-------------------------------------------------

allF:: (a -> Bool) -> [a] -> Bool

allF p xs = foldr (\x y-> p x && y) True xs

-------------------------------------------------

minimumF:: Ord a => [a] -> a

minimumF (x:xs) = foldr min x xs

-------------------------------------------------

mapF:: (a -> b) -> [a] -> [b]

mapF f xs = foldr (\x y -> f x:y) [] xs

-------------------------------------------------

filterF:: (a -> Bool) -> [a] -> [a]

filterF p xs = foldr (\x y -> if p x then x:y else y) [] xs

-------------------------------------------------

takeWhileF:: (a -> Bool) -> [a] -> [a]

takeWhileF p xs = foldr (\x y -> if p x then x:y else []) [] xs

-------------------------------------------------

infixr 5 +++
(+++):: [a] -> [a] -> [a]

xs +++ ys = foldr (:) ys xs


-- Ejercicio 4

foldrN:: (a -> a -> a) -> [a] -> a

foldrN f [x] = x
foldrN f (x:y:xs) = f x (foldrN f (y:xs))

-------------------------------------------------

foldlN:: (a -> a -> a) -> [a] -> a

foldlN f [x] = x
foldlN f (x:y:xs) = foldlN f (f x y: xs)

-- Ejercicio 5

--Apartado b)

paresCuadrados:: (Num a, Enum a) => a -> [(a,a)]

paresCuadrados n = zip [n,n-1..0] (map (^2) [n,n-1..0])

-------------------------------------------------

--Apartado d)

potenciasTres:: Integral a => a -> Int

potenciasTres n = length $ filter (\x -> mod x 100 == 67) $ takeWhile (<n) (iterate (*3) 1)

-------------------------------------------------

--Apartado e)

sumaMultiplos:: Integer

sumaMultiplos = foldr (+) 0 $ filter (\x-> mod x 3 == 0 || mod x 5 == 0) [0..999]
