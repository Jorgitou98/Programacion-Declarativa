-- Ejercicio 1

adivina:: Int -> IO()

adivina n = do print "Introduce tu numero"
               x <- getLine
               y <- return(read x::Int)
               if y == n then do print "Acierto"
                         else if y < n then do print "Es mayor"
                                               adivina n
                                        else do print "Es menor"
                                                adivina n

-- Ejercicio 2
numPalabras:: IO()

numPalabras = do x <- getLine
                 print (length (words x))


-- Ejercicio 3
-- a)
palabras:: String -> IO Int
palabras fileIn = do texto <- readFile fileIn
                     return (length (words texto))

-- b)
palabras':: IO ()
palabras'= do fileIn <- getLine
              npalabras <- palabras fileIn
              print ("El fichero " ++ fileIn ++ " tiene " ++ show npalabras ++ " palabras")

-- c)
promedia :: IO ()

promedia = promediaAux 0 0
 where promediaAux n m = do print ("Dame el numero")
                            x <- getLine
                            y <- return(read x)
                            suma <- return(n + y)
                            divisor <- return (m+1)
                            print ("Suma: " ++ show suma)
                            print ("Media: " ++ show (suma/divisor))
                            if y /= -1 then do promediaAux suma divisor
                                               return ()
                                       else return ()

                            