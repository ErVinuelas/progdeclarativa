--Pablo Martín Viñuelas 



--Ejercicio 1
{-
- a) No porque no tiene un return
- b) IO Char -> Char
- c) IO Char -> IO Char
- d) IO Char -> IO Char o error la verdad que no lo tengo claro
-}
--Ejercicio 2
{-
let x=getChar in [x,x]
No porque no retorna

do x <- getChar
    return x
    return x
No puede haber dos return

do x<-getChar; putStr [x, x]
Ahora sí

do x <- getChar
    putChar x
    putChar x
Sí funciona

do x <- getChar; putChar x ++ putChar
No es una lista

do x <- getChar; putChar (x++x)
putChar no actua sobre listas

do x <- getChar; putStr (x++x)
x++x no funciona con Char
-}

--Ejercicio 3
{-
f 4 = '4'

f :: IO ()

g '4' = '4'\n()

g::IO()
-}
--Ejercicios numerados

--Ej1

palabrasFrase = do x <- getLine; return  $ length(words(x))

--Ej2

palabrasFichero::String -> IO Int
palabrasFichero fileIn = do 
    text <- readFile $ fileIn 
    return $ length $ words text -- Podemos poner let l = (length (words x))
                                               -- return l

--Ej3
palabrasFichero' = do
    print "Dime el susodicho fichero:\n"
    fileIn <- getLine
    putStrLn $ "El fichero " ++ fileIn ++ " tiene " 
    print $ length $ words fileIn
    putStrLn " palabras\n"



--Ej4

getInt :: IO Int
getInt = do 
    line <- getLine
    return (read line::Int)

mostrar::(Fractional a) => Int -> Int -> Int -> (a, a)
mostrar sum cont n = ((fromIntegral (sum + n)) / (fromIntegral (cont + 1)), fromIntegral(sum + n))

promedia:: Int -> Int -> IO ()
promedia sum cont = do 
    putStr "?"
    n <- getInt
    if n == -1 then
        putStrLn "Se acabo"
    else
        do 
            print ( mostrar sum cont n)
            promedia (sum + n) (cont + 1)
            
            
            





    
