--Pablo Martín Vinuelas

--Ejercicio 1

--a)Lista los n primeros n ́umeros naturales pares, ordenada de menor a mayor, usando map.
ej1A::Integer->[Integer]
ej1A n = map (*2) [1..n]

--b)Lista de pares formados por un n ́umero natural como primera componente del par y su
--cuadrado como segunda, ordenados desde el n ́umero natural n hasta 0
ej1B::Integer->[(Integer, Integer)]
ej1B n = zip xs (map (^2) xs) where xs = reverse [0..n]

--c)Lista con las n primeras potencias de 3
ej1C::Int->[Integer]
ej1C n = take n (iterate (*3) 1)

--d)Suma de los n ́umeros menores que n que sean m ́ultiplos de 3 o 5
ej1D::Int->Int
ej1D n = foldr (+) 0 (filter mult3o5 [1..n])
    where mult3o5 x = mod x 3 == 0 || mod x 5 == 0

--e) Primer n ́umero primo mayor que n. Intenta hacerlo de varias formas
--Funcion auxiliar para saber si un numero dado es primo
esPrimo::Integer->Bool
esPrimo n = not $ any (f n) [2..ceiling $ sqrt $ fromInteger $ (n - 1)] 
    where f x y = (mod x y) == 0

ej1E::Integer->Integer
ej1E n = head (filter (esPrimo) [(n + 1)..])

--Ejercicio 2

--iguales f g n m = True si y solo si f x = g x, para todo n ≤ x ≤ m
ej2A::(Eq a, Enum b)=>(b -> a) -> (b -> a) -> b -> b -> Bool
ej2A f g n m = and $ map (h) [n..m]
    where h x = (f x == g x)

--MenorA n m p = menor x con n ≤ x ≤ m que verifica p.c¿Qu ́e ocurre si no existe tal x?
ej2B::(Enum b)=>b->b->(b->Bool)->b
ej2B n m p= head $ filter (p) [n..m]

--Si no existe ese elemento me devuelve excepcion por lista vacia provocada por head

--Menor n p = menor x con x ≥ n que verifica p.
ej2C::(Enum b)=>b->(b->Bool)->b
ej2C n p = head $ filter (p) [n..]

--Si no existe tal x no acaba nunca, porque sigue generando numeros(en algun momento se acabara la memoria asignada y
--entonces nos dira que se ha acabado la memoria)

--La llamada para hallar el primo sería: ej2C (n+1) esPrimo

--MayorA n m p = mayor x con n ≤ x ≤ m que verifica p. 
ej2D::(Enum b)=>b->b->(b->Bool)->b
ej2D n m p = head $ reverse $ filter (p) [n..m]

--Si no existe tal x me devuelve una excepcion por lista vacia, igual que en el 2-c)

--pt n m p = True si y solo si todos los x con n ≤ x ≤ m verifican p.
ej2E::(Enum b)=>b->b->(b->Bool)->Bool
ej2E n m p = and $ map (p) [n..m]

--Ejercicio 3


--a)filter2 xs f g = [us,vs] donde us es la lista resultante de aplicar f a los elementos de
--la lista xs y vs la lista resultante de aplicar g a los elementos de xs.
filter2::[a]->(a->b)->(a->b)->[[b]]
filter2 xs f g = [map f xs, map g xs]


--b)partition p xs = (us,vs), donde us son los elementos de la lista xs que cumplen p y vs
--son el resto.
partition::(a->Bool)->[a]->([a], [a])
partition p xs = (filter (p) xs, filter (not. p) xs)


--c)mapx x [f0,f1,...,fn] = [f0 x,f1 x,...,fn x]
--Funcion auxiliar para aplicar f sobre el argumento x
aplicarSobre::a->(a->b)->b
aplicarSobre x f = f x

mapx::a ->[(a->b)]->[b]
mapx x xs = map (aplicarSobre x) xs


--d) filter1 xss p = [ys1,...,ysn], donde si xss es la lista de listas [xs1,...,xsn], enton-
--ces para todo i, 1 ≤ i ≤ n, ysi es la lista con los elementos de la lista xsi que cumplen la
--propiedad p
filter1::[[a]]->(a->Bool)->[[a]]
filter1 xss p = map (filter p) xss


--e)filters xs ps = [ys1,...,ysn], donde para todo i, 1 ≤ i ≤ n, ysi es la lista con los
--elementos de la lista xs que cumplen la propiedad pi, supuesto que ps=[p1,...,pn].

--Funcion auxiliar para filtrar la lista xs en funcion de p
paraLaLista::[a]->(a->Bool)->[a]
paraLaLista xs p = filter (p) xs

filters::[a]->([a->Bool])->[[a]]
filters xs ps = map (paraLaLista xs) ps