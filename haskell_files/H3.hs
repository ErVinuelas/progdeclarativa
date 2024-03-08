--Pablo Martín Viñuelas


--Ejercicio 1
--a)
hasta20 = [1..20]

a1 = [(\xs n -> map (^n) xs) hasta20 n | n <- [1..10]]

--b)

b1 = [take 10 (iterate (*n) n) | n <- [1..20]]

--c)

c1 = [(n, 2*n) | n <- [(2^n) - 1 | n <- [0..]]]

--d)

d1 = [(\x -> if mod x 2 == 0 then -x else x) n | n <- [1..]]

--Ejercicio 2

--a)

a2 = let divide x y = (mod x y == 0) in 
    [(n, (\x -> filter (divide x) [1..x - 1]) n)| n <- [19..50] ]

--b)

b2 = let divide x y = (mod x y == 0) in 
    [n| n <- [1..1000],  (n == sum ((\x -> filter (divide x) [1..x - 1]) n))]

--c)
c2::Int -> Int -> [Int]
c2 menor mayor = let divide x y = (mod x y == 0) in 
    [n| n <- [menor..mayor],  (n == sum ((\x -> filter (divide x) [1..x - 1]) n))]


{--
Ejercicio 3

funcionZ::Enum -> [Enum ] ->Enum
funcionZ x y = x * last y

funcionG::Enum -> Enum -> Enum -> Enum
funcionG x z u = (x + z)*u

funcionLanda::[Enum]->[(Enum, Enum)]
funcionLanda x y z u = (funcionG x z u, funcionG x z (u + 1))

f::Enum -> [Enum] -> [Enum] 
f x y =  map (funcionLanda x y (funcionZ x y) ) y
--}

--Solucion Sara

g::Num a => a -> [a] -> a -> a
g x y u = (x + (x*last y) * u

h::Num a => a -> [a] -> a -> (a,a)
h x y = \u -> (g x y u , g x y (u + 1)
f::Num a => a -> a -> [a]
f x y = map (h x y) y


--Ejercicio 4
f n = map (^2) $ filter (mod x 2 == 0) [1..n]

g p n m = map f $ outerProduct [1..n] [1..m]
where f (x,y) = x + y

outerProduct xs ys =
   do
       x <- xs          -- for each x drawn from xs:
       y <- ys          --   for each y drawn from ys:
       return if x <= y then (x,y) else null     --      produce the (x,y) pair


h p q n m = map f $ filter h $ outerProductBigger [1..n] [1..m]
where f (x,y) = x + y
where h (x, y) = (p (n-x) && q y)

--Solucion Pablo Heredero
{--
1) h p q n m = concat [[x + y | y <-[x..m], q y ]| x <-[1.n], p(n-x)]
2) hpq n m = concat(map f(filter(\x -> p(n-x))[1..n]))
    where f x = (map (\y->x+y)(filter q [x..m]))
--}

--Ejercicio 5
minimoDesde::(Int->Bool)->Int->Int
minimoDesde p n = head [x | x <- [n ..], p x]   

paresHasta::Int->[Int]
paresHasta n = [n | n <- [0..n], mod n 2 == 0]

mezaclaParImpar::[Int]->[Int]->[(Int, Int)]
mezclaParImpar xs ys = [(x,y) | x <- xs, y <- ys, mod x 2 == 0 && mod y 2 == 1]

prefijos::[Int]->[[Int]]
prefijos xs = [take n xs | n <- [0..lenght xs]

