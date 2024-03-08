--Pablo Martín Viñuelas


--Ejercicio 1
cantidad::Integer
cantidad = 10 ^ 6

segMinuto::Integer
segMinuto = 60

segHora::Integer
segHora = 60 * segMinuto

segDia::Integer
segDia = 24 * segHora

segAno::Integer
segAno = 365 * segHora

resultado::(Integer, Integer, Integer, Integer, Integer)
resultado = 
    let
        anos = div cantidad segAno;
        restanteAnos = mod cantidad segAno;
        dias = div restanteAnos segDia
        restanteDias = mod restanteAnos segDia
        horas = div restanteDias segHora
        restanteHoras = mod restanteDias segHora
        minutos = div restanteHoras segMinuto
        restanteMinutos = mod restanteHoras segMinuto
        segundos = restanteMinutos
    in (segundos, minutos, horas,dias, anos);

resultadoFunc::Integer->(Integer, Integer, Integer, Integer, Integer)
resultadoFunc cantidadInicial =
    let
        anos = div cantidadInicial segAno;
        restanteAnos = mod cantidadInicial segAno;
        dias = div restanteAnos segDia
        restanteDias = mod restanteAnos segDia
        horas = div restanteDias segHora
        restanteHoras = mod restanteDias segHora
        minutos = div restanteHoras segMinuto
        restanteMinutos = mod restanteHoras segMinuto
        segundos = restanteMinutos
    in (segundos, minutos, horas,dias, anos);

--Ejercicio 2

esBisiestoIf::Integer->Bool
esBisiestoIf ano =
    if (mod ano 4) == 0 then True else
    if (mod ano 400) == 0 then True else
    False

esBisiestoGuarda::Integer->Bool
esBisiestoGuarda ano
    | (mod ano 4) == 0 = True
    | (mod ano 400) == 0 = True
    | otherwise = False
    
--Ejercicio 3

media::(Fractional a) => [a]-> a
media xs = 
    let
        long = length xs ;
        suma = sum xs
    in suma / (fromIntegral long)


--Ejercicio 4 a)


num_digitos :: Integer -> Integer
num_digitos x 
    | x < 10 = 1
    | otherwise = 1 + (num_digitos(div x 10))


--Ejercicio 4 b)

--Funcion auxiliar para sumar las cifras del numero entero
suma_digitos :: Integer -> Integer
suma_digitos x
    | x < 10 = x
    | otherwise = suma_digitos (div x 10) + (mod x 10)

reduccion :: Integer -> Integer
reduccion x = 
    let sum = suma_digitos x 
    in 
        if sum < 10 then sum else
            reduccion sum

--Ejercicio 5

--Estricta en el primer argumento
disyuncion_booleana1 :: Bool -> Bool -> Bool
disyuncion_booleana1 False True = True
disyuncion_booleana1 False False = False
disyuncion_booleana1 True _ = True
    
--Estricta en el segundo argumento
disyuncion_booleana2 :: Bool -> Bool -> Bool
disyuncion_booleana2 True False = True
disyuncion_booleana2 False False = False
disyuncion_booleana2 _ True = True



--Estrict en las dos variables
disyuncion_booleana3 :: Bool -> Bool -> Bool
disyuncion_booleana3 False False = False
disyuncion_booleana3 True False = True
disyuncion_booleana3 False True = True
disyuncion_booleana3 True True = True        
        
