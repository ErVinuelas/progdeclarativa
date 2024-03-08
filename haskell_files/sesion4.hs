--Pablo Martín Viñuelas


--Ejercicio 1

type Coordenada = Int
type Punto = (Coordenada, Coordenada)

data Direccion = Arriba | Abajo | Derecha | Izquierda  deriving (Eq, Ord, Show)


mueve::Punto -> Direccion -> Punto
mueve (x, y) m 
    | (x < 0 || x > 100 || y < 0 || y > 100) = error("El punto está fuera de rango.\n")
    | m == Arriba = if y + 1 < 100 then (x, y + 1) else (x, y)
    | m == Abajo = if y - 1 > -1 then (x, y - 1) else (x, y)
    | m == Derecha = if x + 1 < 100 then (x + 1, y) else (x, y)
    | m == Izquierda = if x - 1 > -1 then (x - 1, y) else (x, y)


destino::Punto -> [Direccion] -> Punto
destino p ms = foldl mueve p ms

trayectoria:: Punto -> [Direccion] -> [Punto]
trayectoria p ms = foldl (\x y -> let z = (mueve (last x) y) in x++[z]) [p] ms


--Ejercicio 2

data Nat = Cero | Suc Nat deriving (Eq, Ord)

infixl 4 +++
n +++ Cero = n 
n +++ (Suc x) = Suc(x +++ n)

infixl 4 ***
n *** (Suc Cero) = n
n *** (Suc x) = n +++ (x***n)


natToInt::Nat -> Int
natToInt Cero = 0
natToInt (Suc x) = 1 + natToInt x

instance Show Nat where
    show n = show (natToInt n)


--Ejercicio 3

data Complejo = Complejo Double Double deriving (Eq)

instance Show Complejo where
    show (Complejo x y) = if y > 0 then show $ "["++(show x)++"+"++(show y)++"i]" else show $ "["++(show x)++(show y)++"i]"

instance Num Complejo where
    (Complejo a b) + (Complejo c d) = (Complejo (a+c) (b+d))
    (Complejo a b) * (Complejo c d) = (Complejo (a*c-b*d) (a*d+b*c))
    (Complejo a b) - (Complejo c d) = (Complejo (a-c) (b-d))

instance Fractional Complejo where
    (Complejo a b) / (Complejo c d) = (Complejo ((a*c + b*d)/(c*c+d*d)) ((b*c - a*d)/(c*c + d*d)))


--Ejercicio 4

class Medible a where
    medida::a -> Int

instance Medible Bool where
    medida True = 1
    medida False = 0


instance (Medible a) => Medible [a] where
    medida [] = 0
    medida (x:xs) = medida x + medida xs

instance (Medible a, Medible b) => Medible (a, b) where
    medida (x,_) = medida x
