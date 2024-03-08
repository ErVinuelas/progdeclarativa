--Pablo Martín
--Ejercicio 1 de la Hoja 2 de Programación declarativa

--Funciones auxiliares:
esPrimo::Integer->Bool
esPrimo n = not $ any (f n) [2..(floor $ sqrt $ fromInteger (n - 1))] where f x y = (mod x y) == 0 

--a)La lista de los n primeros números naturales pares

primerosParesHasta::Integer -> [Integer]
primerosParesHasta n = [2,4..(2*n)]

--Como funcion recursiva
pares::Int->[Int]
pares 0 = []
pares n
	| 
--b) La lista de los pares formados por un num como primera componente
--y su cuadrado como segunda desde n hasta cero

--ej2::Integer->[(Integer, Integer)]
--ej2 n = zip xs (map (^2) xs) where xs = reverse [0..n]

ej2::Integer->[(Integer, Integer)]
ej2 n 
	| n == 0 = [0,0]
	| otherwise = (n, n^2) : ej2 n-1

--c) La suma que aparece que no se como ponerla
f::Floating->Floating
f x = x * (cos x)

ej3::Integer->Floating
ej3 n = sum $ map f [1..n]

--Recursion

ej3::Integer->Floating
ej3 n 
	| 0 = 0
	| n > 0 = n * (cos n) + ej3 n-1

--d)La suma de los numeros menosres que n que sean multiplos de 3 o 5

ej4::Integer->Integer
ej4 n = sum $ filter mult3o5 [1..n] where mult3o5 x = mod x 3 == 0 || mod x 5 == 0

--Recursion
ej4::Integ

--e) El num de potencias de 3 que sean menores de n y acaben en 43. Puedes usar funciones
-- auxiliares

ej5::Integer->Int
ej5 n = length $ filter (menoresNyAcaban43) (map (3^) [1..n]) where menoresNyAcaban43 x = (x < n) && (mod x 100 == 43)
