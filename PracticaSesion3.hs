-- Jorge Villarrubia


----------------------------------------
-- Ejercicio 1

-- Apartado a)

generaLista :: Integral a => [a]
generaLista = concat [ [i, -i] | i <- [1..]]

-- Apartado b)
parejas::Integral a => [(a, a)]
parejas =[(x,y) | z <-[0..], x <- [0..z], y <- [0..z], x + y == z ]

----------------------------------------

-- Ejercicio 2

-- Apartado a)

sufijos:: [a] -> [[a]]
sufijos xs = [drop n xs | n <- [0..length xs]]

----------------------------------------

--Aparatado b)
sublists:: [a]-> [[a]]

sublists xs = []:[take n xs| xs <- (sufijos xs), n <- [1..length xs]]

----------------------------------------

-- Apartado c)
perms:: Eq a => [a]-> [[a]]
perms [x] = [[x]]
perms xs = [x:y | x <- xs, y <- perms( takeWhile (/= x) xs ++ tail (snd(span (/= x) xs))) ]

----------------------------------------

-- Aparatado d)

-- Para sumandos 3 por ejemplo scaaria tanto [1,2] como [2,1]

sumandos 0 = [[]]
sumandos n = [ x:ys |  x <- [1..div n 2], ys <- sumandos (n-x)  ]