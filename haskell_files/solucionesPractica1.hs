--Reduccion segun Sara
reduccion :: Integer -> Integer
reduccion n 
    | absoluto < 10 = absoluto
    | otherwise = reduccion (div absoluto 10 ) + (mod absoluto 10)
where absoluto = abs n

--Ejercicio 1
--No conviene declararlo como constantes
let min = 10 ^ 6 div 60 in 
    let [..]


media::Fractional -> Fractional
media xs =
    sum xs / fromIntegral(length xs)
