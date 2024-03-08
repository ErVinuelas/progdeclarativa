--Pablo Martín Viñuelas


--1)
--Voy recorriendo la lista pero descartando los que no son el primer elemento que he cogido
lastRedef::[a]->a
lastRedef (x:xs) = foldl (\x y -> y) x xs

--Recorro del ultimo al primero y lo pongo en la cola de los que llevo de manera que le doy la vuelta
reverseRedef::[a]->[a]
reverseRedef xs = foldr (\x ys -> ys++[x]) [] xs

--Doy valor inicial cero y voy haciendo una OR de todos los valores. Daría igual el orden en que recorra
anyRedef::Foldable t =>(a->Bool)-> t a -> Bool
anyRedef p xs = foldl (\x y -> x || p y) False xs

--Tomo el primer valor de la lista y me quedo siempre con el más pequeño mientras recorro la lista
minimumRedef::(Ord a) =>[a] -> a
minimumRedef (x:xs) = foldl (\z y -> if z < y then z else y) x xs

--Recorro la lista y voy llevando una lista con los elementos a los que he ido aplicando la funcion
mapRedef::(a->b)->[a]->[b]
mapRedef p xs = foldr (\y z -> [p y]++z) [] xs

--Vamos acumulando una tupla, el primer elemento se corresponde con la lista que vamos a devolver(por completar) y el
--segundo es un booleano que indica si hemos encontrado un elemento que no cumple p.
--Si esto ocurre dejamos de anadir elementos a la lista. Finalmente filtramos la lista y la devolvemos
--Nótese que si en el if cambiamos la condicion por p y && cont, cuando haya un elemento undefined nos devolverá error si no hemos
--encontrado un elemento que no cumpla p. EJ: takeWhileRedef (<0) [1,2,3,4,undefined].
takeWhileRedef::(a->Bool)->[a]->[a]
takeWhileRedef p xs = (\(lista, other) -> lista) $ foldl (\(ys,cont) y -> if cont && p y then (ys++[y],True) else (ys, False)) ([], True) xs


--2)
--Tengo una lista de los naturales y los recorro. Los que son pares devuelvo su negatiovo y sino los dejo igual. Los voy añadiendo
-- a una lista que es la que voy acumulando.
listaej2 = foldl (\z y -> z++[f y]) [] [1..100]
    where f j = if mod j 2 == 0 then -j else j

--3)
-- Itero sobre la lista [1] y añado una entrada más con el elemento que tenía +1 a la vez que aplico un +1 sobre los elementos de la lista
listaej3 = iterate (\(x:xs) -> (map (+1) ([x]++xs))++[x+1]) [1]

--4)
--Recorro todos los naturales(n) y por cada uno de ellos considero todos los menores o iguales(que x) y consideros los pares (x, n-x)
listaej4 = [(x,n-x) | n<-[0..], x<-[0..n]]

--5)
--Los casos de lista vacia y con un elemento los tratamos aparte. 
--Consideramos la lista de listas de permutaciones de xs, las permutaciones totales
--nos vienen dadas por considerar todas las posibles posiciones que puede tomar x en cada una de estas listas
--(de la 0 hasta la longitud de la lista xs).
permuta::[a]->[[a]]
permuta [] = [[]]
permuta [x] = [[x]]
permuta (x:xs) = [(\ys zs -> ys++[x]++zs) (take n ts) (drop n ts) | n <- [0..(length xs)], ts <- permuta xs] 

--6)
--Dada una lista no vacia llevamos acumulada en el fold una terna = (elemento anterior, lista de la cuesta actual, entero por el que estamos en la lista) si el elemento actual 
-- cumple la condicion de cuesta, añadimos el par de (actual, entero) y seguimos procesando. Al final filtramos la terna y devolvemos solo la lista final.
cuestaPos::(Ord a)=>[a]->[(a, Int)]
cuestaPos (x:xs) = (\(h,resultado,k) -> resultado) $ foldl (\(ant, ys, num) act -> (if ant < act then (act, ys++[(act,num+1)], num+1) else (act, ys, num+1))) (x, [], 0) xs