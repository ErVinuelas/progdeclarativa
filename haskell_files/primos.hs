--Pablo Martín
--Programa para saber si un número es primo

esPrimo::Integer->Bool
esPrimo n = not $ any (f n) [2..(floor $ sqrt $ fromInteger (n - 1))] where f x y = (mod x y) == 0 