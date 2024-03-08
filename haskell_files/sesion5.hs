--Pablo Martín Viñuelas

--Funciones auxiliare

getInt :: IO Int
getInt = do 
    line <- getLine
    return (read line::Int)


--Ejercicio 1

adivina::Int -> IO ()
adivina n = do
                putStrLn  "Adivina el número:\n"
                putStr "?"
                num <- getInt
                if(num == n) then
                    do 
                    putStr  "Lo has adivinado!\n"
                    return ()
                else if (n > num) then
                    do 
                    putStr  "El n es mayor que lo que has escrito\n"
                    adivina n
                else
                    do 
                    putStr  "El n es menor que lo que has escrito\n"
                    adivina n
                    
                    
                    
--Ejercicio 2

