--Pablo Martín Viñuelas

--Práctica final de Haskell -- Programación Declarativa

{-
-------***--------
  Primera Parte
------------------
-}
--Definición del tipo de datos Prop 

data Prop = P | Q | R | S deriving (Show, Read, Eq)

--Definición del tipo de datos Formula

data Formula = Atom Prop | Not Formula | Or Formula Formula | And Formula Formula deriving (Read, Eq)

instance Show Formula where
    show (Atom p) = show p
    show (Not f) = "¬" ++ show f
    show (Or f1 f2) = "(" ++ show f1 ++ " ∨ " ++ show f2 ++ ")"
    show (And f1 f2) = "(" ++ show f1 ++ " ^ " ++ show f2 ++ ")"


--Conjunto de formulas para ejemplos y comprobaciones

--Las que aparecen en el enunciado:

-- f1 = (P ∧ ¬Q) ∨ ¬S
f1 = (Or (And (Atom P) (Not (Atom Q))) (Not (Atom S))) 

--f2 = (¬R ∨ P ) ∧ ¬(Q ∨ ¬P )
f2 = (And (Or (Not (Atom R)) (Atom P)) (Not (Or (Atom Q) (Not (Atom P))))) 

--f3 = (S ∧ Q) ∧ (¬P ∨ R) ∧ S
f3 = (And (And (And (Atom S) (Atom Q)) (Or (Not (Atom P)) (Atom R))) (Atom S)) 

-- Propias para pruebas:

-- f4 = (P ∨ ¬Q ∨ R)
f4 = (Or (Or (Atom P) (Not (Atom Q))) (Atom R))

-- f5 = (P ∨ ¬Q) ^ Q ^ ¬P
f5 = (And (Or (Atom P) (Not (Atom Q))) (And (Atom Q) (Not (Atom P))))

--f6 = (¬P ^ P)
f6 = (And (Atom P) (Not (Atom P)))

    
--Dada una expresión comprueba si es o no clausula(disyutiva)

esClausula::Formula->Bool
--Casos base
esClausula (Atom f) = True
esClausula (Not (Atom f)) = True
esClausula (And _ _) = False
--Caso recursivos
esClausula (Or f1 f2) = (esClausula f1) && (esClausula f2)



--Dada una formula en FNC devuelve una lista con las componentes de la conjuncion

fncAlista::Formula -> [Formula]
fncAlista (And f1 f2) = (fncAlista f1) ++ (fncAlista f2)
fncAlista f = if esClausula f then [f] else error "La fórmula proporcionada no está en FNC."

--Dada una cláusula devuelve la lista de los literales que la forman

clausulaLista::Formula->[Formula]
clausulaLista (Or f1 f2) = (clausulaLista f1) ++ (clausulaLista f2)
clausulaLista f = [f] 

--Devuelve uno si la formula es atomica, cero si es una negación de una atómica y error en otro caso

conteaSiEsAtom::Formula -> Int
conteaSiEsAtom (Atom p) = 1
conteaSiEsAtom (Not (Atom p)) = 0
conteaSiEsAtom _ = error "La fórmula no es atómica ni una negación de atómica"

--Funcion que contabiliza los literales positivos en una clausula

contabilizaLitPosit::Formula -> Int 
contabilizaLitPosit f = sum $ map conteaSiEsAtom (clausulaLista f)

--Dada una formula devuelve si es de Horn
esClausulaHorn::Formula -> Bool
esClausulaHorn f = (esClausula f) && (sum (map (contabilizaLitPosit) (clausulaLista f)) <= 1)  


--Suprime de la lista el primer elemento que es igual a otro dado, devuelve la lista modificada y si se ha modificado

quitarPrimerCoinc::(Eq a) => a -> [a] -> ([a], Bool)
quitarPrimerCoinc f [] = ([], False)
quitarPrimerCoinc f (p:xs) = if (f == p) then 
                                (xs, True) 
                            else 
                                let res = quitarPrimerCoinc f xs in (p:(fst(res)), snd(res))

--Dadas dos expresiones xs1 y xs2, que son listas de literales asociadas a clausulas que se pueden resolver, devuelve la lista de literales asociada al resolvente de dichas clausulas.

resolvente::[Formula] -> [Formula] -> [Formula]
resolvente [] xs = xs
resolvente ((Atom p):xs1) xs2 = let (xs2Modf, found) = quitarPrimerCoinc (Not (Atom p)) xs2; nextRes = resolvente xs1 xs2Modf in 
                                    if found then 
                                        nextRes 
                                    else 
                                        (Atom p):nextRes
                                        
resolvente ((Not (Atom p)):xs1) xs2 = let (xs2Modf, found) = quitarPrimerCoinc (Atom p) xs2; nextRes = resolvente xs1 xs2Modf in 
                                    if found then 
                                        nextRes 
                                    else (Not (Atom p)):nextRes

resolvente (p:xs1) xs2 = p:(resolvente xs1 xs2)

{-
-------***--------
  Segunda Parte
------------------
-}

--Funciones auxiliares para obtener los elementos de una tupla de tres elementos

first::(a,b,c) -> a
first (a,_,_) = a

second::(b,a,c) -> a
second (_,a,_) = a

third::(b,c,a) -> a
third (_,_,a) = a


--Funcion auxiliar para buscar en la lista S un conjunto de clausulas que nos proporcione una nueva resolvente(segundo elemento), también 
--devuelve el conjunto S sin la formula que hemos usado para la resolvente y si hemos podido llevar a cabo la operación(tercer elemento)

buscarPosRes::[[Formula]] -> [Formula] -> ([[Formula]], [Formula], Bool)
buscarPosRes [] g = ([], [], False)
buscarPosRes (l:xs) g = 
    if (any (\x -> elem (Not x) g) l) then 
        (xs, l, True) 
    else 
        let    
            siguienteIt = buscarPosRes xs g; 
            nuevaXs = first siguienteIt; 
            cs = second siguienteIt;  
            encontrado = third siguienteIt 
        in 
        (l:nuevaXs, cs, encontrado)

--Devuelve la lista resultante de aplicar el algoritmo de resolución

resolucion::[[Formula]] -> [Formula] -> [Formula]
resolucion xs [] = []
resolucion xs g =
    if encontrado then
        resolucion nuevaXs resCG
    else
        g
    where   
        (nuevaXs, cs, encontrado) = buscarPosRes xs g;
        resCG = resolvente cs g 

{-
-------***--------
  Tercera Parte
------------------
-}

{-
El programa interactivo recibe al usuario y le pide que elija una opción de las dos que aparecen. En ellas hacemos uso de todas las funciones que hemos
definido a lo largo de la práctica. La opción A recoge una formula en forma FNC y la procesa aplicando el algoritmo de resolucion. La opción B recoge un conjunto
de clausulas S y otra G (que se adaptan a las definidas en el enuncidado) y aplica el algoritmo de resolucion sobre ellas.
-}

--Comprueba si una clausula de Horn contine algun termino positivo

tienePositv::[Formula] -> Bool
tienePositv [] = False
tienePositv ((Atom p):xs) = True
tienePositv ((Not (Atom p)):xs) = tienePositv xs


--Divide las clausula en dos grupos, las que tienen un literal no negativo y los que no 
             
dividirClausulas::[[Formula]] -> ([[Formula]], [[Formula]])
dividirClausulas [] = ([], [])
dividirClausulas (f:xs) = let resSiguiente = dividirClausulas xs in 
                            if tienePositv f then
                                (f:(fst resSiguiente ), snd resSiguiente ) 
                            else 
                                (fst resSiguiente, f:(snd resSiguiente))            
    

--Programa que aplica al conjunto la resolucion sucesivamente. El primer conjunto de formulas son las que todas son negativas y el segundo con una positiva.

aplicarLaResolucion::[[Formula]] -> [[Formula]] -> [[Formula]] 
--Casos base
aplicarLaResolucion [] [] = []
aplicarLaResolucion [[]] [[]] = []
aplicarLaResolucion [] xs = xs
aplicarLaResolucion [[]] xs = xs
aplicarLaResolucion xs [] = xs
aplicarLaResolucion xs [[]] = xs
--Casos recursivos
aplicarLaResolucion (f:xs) ys = let (nuevaYs, cs, encontrado) = buscarPosRes ys f;  in
                                    let resfYs = resolvente cs f in
                                        if encontrado then
                                            aplicarLaResolucion (resfYs:xs) nuevaYs
                                        else 
                                            aplicarLaResolucion xs ys
  
--Programa principal que aplica la refutacion a un conjunto de clausulas de Horn.

refutar::[[Formula]] -> ([[Formula]], Bool)
refutar xs = let (listaUnPositv, listaSinPositv) = dividirClausulas xs; res = aplicarLaResolucion listaSinPositv listaUnPositv in 
                if (length res) == 0 then
                    (res, True)
                else 
                    (res, False)
    
    
--FUNCIONES DE ENTRADA/SALIDA

mensajeInicial = "Bienvenido. Introduzca el caracter de la opción que desee llevar a cabo:\nA) Introducir una fórmula en FNC y tratar de refutarla.\nB) Introducir un conjunto S de clausulas de Horn con un literal positivo y una G clausula de Horn con los literales negativos y aplicar el algoritmo de resolución.\nPor defecto se escoge la B."

--Programa interactivo que recoge una formula de la entrada de teclado

getFormula::IO Formula
getFormula =
    do 
        fStr <- getLine
        return ((read fStr)::Formula)

--Recoge una formula en FNC

introducirFormulaFNC::IO [[Formula]]
introducirFormulaFNC =
    do 
        f <- getFormula
        
        putStrLn "La formula introducida es:"
        putStrLn (show f)
        
        let listaConj = fncAlista f in
            if not $ all (esClausulaHorn) (listaConj) then
                do 
                    putStrLn "La fórmula introducida no se encuentra como conjución de clausulas de Horn. Inténtelo de nuevo:"
                    introducirFormulaFNC
            else
                return [clausulaLista x | x <- listaConj]
 
--Interaccion con el usuario para introducir las formulas para el algoritmo de resolucion

introducirClausulasParaResolucion::IO ([[Formula]], [Formula])
introducirClausulasParaResolucion =
    do
        putStrLn "Introduzca la primera clausula de S con un solo literal positivo"
        f <- getFormula
        putStrLn "¿Desea introducir otra formula para S?[s/n](default no)"
        res <- getChar
        if res == 's' then
            do 
                otrasForm <- introducirClausulasParaResolucion 
                return (((clausulaLista f):(fst otrasForm)), snd otrasForm)
        else
            do
                putStrLn "Introduzca la fórmula G con todos los literales negativos:"
                g <- getFormula
                return ([clausulaLista f], (clausulaLista g))
            
--Interaccion para procesar el caso en que el usuario elija la opcion A

opcionFNC::IO ()
opcionFNC =
    do 
        putStrLn "Introduzca la fórmula que quiere refutar en forma FNC donde las disyunciones están en forma de Horn:"
        xs <- introducirFormulaFNC
        putStrLn "------------------------------------------------------------------------------------"
        putStrLn "Las fórmula se ha introducido correctamente, aplicamos el algoritmo de resolucion:"
        putStrLn "------------------------------------------------------------------------------------"
        let res = refutar xs in
            if (0 == length res) then 
                putStrLn "Las fórmula se ha refutado. Éxito"
            else
                do 
                    putStrLn "No se ha podido refutar. Hemos llegado a:"
                    print $ fst res

--Interaccion para procesar el caso en que el usuario elija la opcion B  

opcionResolucion::IO()
opcionResolucion =
    do 
        (xs, g) <- introducirClausulasParaResolucion
        putStrLn "------------------------------------------------------------------------------------"
        putStrLn "Las fórmulas se han introducido correctamente, aplicamos el algoritmo de resolucion:"
        putStrLn "------------------------------------------------------------------------------------"
        
        let res = resolucion xs g in
            if (0 == length res) then 
                putStrLn "Las fórmulas se han refutado. Éxito"
            else
                do 
                    putStrLn "No se han podido refutar. Hemos llegado a:"
                    print res
                
        
--Programa principal para la interaccion con el usuario, depliega las opciones del menu

principalIO::IO()
principalIO =
    do
        putStrLn mensajeInicial
        putChar '\n'
        res <- getChar
        putChar '\n'
        if (res == 'A') || (res == 'a') then
            opcionFNC
        else 
            opcionResolucion
          


