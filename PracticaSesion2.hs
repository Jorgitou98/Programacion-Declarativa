-- Jorge Villarrubia Elvira

cuadrados:: (Num a, Eq a) => a -> [a]

cuadrados 0 = [0]
cuadrados n = cuadrados (n-1) ++ [n^2]

cuadradosPares:: (Num a, Eq a) => a -> [(a,a)]

cuadradosPares 0 = [(0,0)]
cuadradosPares n = cuadradosPares (n-1) ++ [(n,n^2)]

suma:: (Floating a, Eq a) => a

suma = sumatorio 100
 where sumatorio 1 = abs(sin(1))
       sumatorio n = sumatorio (n-1) + n * abs(sin(n))

potencias:: Integral a => a -> a

potencias n = potenciasAux 0 n
 where potenciasAux x n
        | (3^x) > n = 0
        | (mod (3^x) 100 == 67) = 1 + potenciasAux (x+1) n
        | otherwise = potenciasAux (x+1) n


multiplos:: Integral a => a

multiplos = multiplosMen 999
 where multiplosMen 1 = 0
       multiplosMen n = if (mod n 3 == 0 || mod n 5 == 0)
                        then n + multiplosMen (n-1)
                        else multiplosMen (n-1)

filter2:: [a] -> (a -> Bool) -> (a -> Bool)-> ([a], [a])

filter2 xs p q = (filter p xs, filter q xs)

filters:: [a] -> [(a -> Bool)] -> [[a]]

filters xs ps = map (\x -> filter x xs) ps

mapx:: a -> [(a -> b)] -> [b]

mapx x fs = map (\y -> y x) fs

iguales:: Eq a => (Int -> a) -> (Int -> a) -> Int -> Int -> Bool

iguales f g m n = all (\x -> f x == g x) (take (n+1-m) (iterate (+1) m))
