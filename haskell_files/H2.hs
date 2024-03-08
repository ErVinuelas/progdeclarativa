--Pablo Martín Viñuelas

--2)

--a)
zip3::[a]->[b]->[c]->[(a,b,c)]
zip3 xs ys zs = map $ (\(x,(y,z)) -> (x,y,z)) zip $ xs zip ys zs

--b)
imparesEn::[Int]->[Int]
imparesEn xs = filter (/x -> (mod x 2 == 0)) xs

--c)
escalar::[Int]->[Int]->Int
escalar = sum $ zipWith (*)

--d) 
mcdList::[Int]->Int
mcdList x:xs = foldr gcd x xs


