
-- 1 Programá las siguientes funciones
-- ► (a)
esCero :: (Eq a, Num a) => a -> Bool
esCero x = x == 0 

-- Ejemplo esCero 0 = True, esCero 1 = False

-- ► (b)
esPositivo :: (Ord a, Num a) => a -> Bool
esPositivo x = x > 0

-- Ejemplo esPositivo 1 = True, esPositivo (-1) = False

-- ► (c)

esVocal :: Char -> Bool
esVocal x | x == 'a' = True
          | x == 'e' = True
          | x == 'i' = True
          | x == 'o' = True
          | x == 'u' = True
          | otherwise = False
-- Ejemplo esVocal 'a' = True, esVocal 'b' = False

esVocal2 :: Char -> Bool
esVocal2 x = x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u'

-- ► (d)
valorAbsoluto :: (Ord a, Num a) => a -> a
valorAbsoluto x | x >= 0 = x
                | otherwise = -x

-- Ejemplo valorAbsoluto 5 = 5, valorAbsoluto (-5) = 5

-- 2 Programá la siguiente funciones usando recursión o composición
-- ► (a)
paraTodo :: [Bool] -> Bool
paraTodo [] = True
paraTodo (x:xs) = x && paraTodo xs 

-- Ejemplo paraTodo [True, False, True] = False, paraTodo [True, True, True] = True

-- ► (b)
sumatoria :: Num a => [a] -> a
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- Ejemplo sumatoria [1,2,3] = 6


-- ► (c)
productoria :: Num a => [a] -> a
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- Ejemplo productoria [] = 1, productoria [1,2,3] = 6

-- ► (d)
factorial :: (Eq t, Num t) => t -> t
factorial 0 = 1
factorial n = n * factorial (n-1)

-- Ejemplo factorial 0 = 1, factorial 3 = 6

-- ► (e)
promedio :: Fractional a => [a] -> a
promedio xs = sumatoria xs / fromIntegral (length xs)

-- Ejemplo promedio [1,2,3] = 2.0

-- 3 Programá la función pertenece :: Int -> [Int] -> Bool, que verifica si  un numero se encuentra en una lista.
pertenece :: Eq t => t -> [t] -> Bool
pertenece x [] = False
pertenece x (y:ys) = x == y || pertenece x ys

-- Ejemplo pertenece 3 [1,2,3] = True, pertenece 4 [1,2,3] = False

-- 4 Programá las funciones que implementan los cuantificadores generales. Notá que el segundo parámetro de cada función, es otra función. 

-- ► (a) 
paraTodo' :: [a] -> (a -> Bool) -> Bool
paraTodo' [] f = True
paraTodo' (x:xs) f = f x && paraTodo' xs f

-- Ejemplo paraTodo' [1,2,3] even = False, paraTodo' [2,4,6] even = True

-- ► (b)
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] f = False
existe' (x:xs) f = f x || existe' xs f

-- Ejemplo existe' [1,2,3] even = True, existe' [1,3,5] even = False

-- ► (c)
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] f = 0
sumatoria' (x:xs) f = f x + sumatoria' xs f

-- Ejemplo sumatoria' [1,2,3] id = 6

-- ► (d)
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] f = 1
productoria' (x:xs) f = f x * productoria' xs f

-- Ejemplo productoria' [1,2,3] id = 6

--5 Definí la función paratodo, pero usando la función paraTodo' sin recursión ni análisis por casos.
paraTodo'' :: [Bool] -> Bool
paraTodo'' xs = paraTodo' xs id
-- paraTodo' toma una funcion y como debemos retornar un booleano le pasamos la funcion identidad
-- It's useful as an argument to higher order functions (functions which take functions as arguments), where you want some particular value left unchanged.

-- 6
-- ► (a) todosPares :: [Int] -> Bool, verificá que todos los numeros de una lista son pares.
todosPares :: [Int] -> Bool
todosPares xs = paraTodo' xs even

-- Ejemplo todosPares [1,2,3] = False, todosPares [2,4,6] = True

-- ► (b) hayMultiplo :: Int -> [Int] -> Bool, verificá si hay algún múltiplo de un numero en una lista.
hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n xs = existe' xs (\x -> mod x n == 0)
-- mod x n == 0 verifica si x es multiplo de n.

-- Ejemplo hayMultiplo 2 [1,2,3] = True, hayMultiplo 3 [1,2,3] = False

-- ► (c) sumaCuadrados :: Int -> Int, calcula la suma de los cuadrados de los primeros n números.
sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' [1..n] (\x -> x^2)
-- [1..n] es una lista de 1 a n.
-- x^2 eleva x al cuadrado.


-- Ejemplo sumaCuadrados 3 = 14, sumaCuadrados 4 = 30

-- ► (d) existeDivisor :: Int -> Int -> Bool, verifica si el primer numero es divisor del segundo.

existeDivisor :: Int -> [Int] -> Bool
existeDivisor n xs = existe' xs (\x -> mod n x == 0)
-- [1..m] es una lista de 1 a m (siempre y cuando m sea mayor a 1).
-- mod m x == 0 verifica si x es divisor de m.
-- x == n verifica si x es igual a n.

-- Ejemplo existeDivisor 2 4 = True, existeDivisor 3 4 = False

-- ► (e) esPrimo :: Int -> Bool, verifica si un numero es primo.
esPrimo :: Int -> Bool
esPrimo n = not (existeDivisor n [2..n-1])
-- [2..n-1] es una lista de 2 a n-1.
-- not niega el resultado de existeDivisor.

-- Ejemplo esPrimo 5 = True, esPrimo 6 = False

-- ► (f) factorial' :: Int -> Int, calcula el factorial de un numero.
factorial' :: Int -> Int
factorial' n = productoria' [1..n] id

-- Ejemplo factorial' 3 = 6

-- ► (g) multiplicarPrimos :: [Int] -> Int, calcula el producto de los numeros primos de una lista.
multiplicarPrimos :: [Int] -> Int
multiplicarPrimos xs = productoria' xs (\x -> if esPrimo x then x else 1)
-- if es una expresion condicional, if condicion then expresion1 else expresion2
-- if esPrimo x then x else 1 verifica si x es primo, si lo es devuelve x, sino devuelve 1.

-- Ejemplo multiplicarPrimos [1,2,3,4,5] = 30

-- ► (h) esFibonacci :: Int -> Bool, verifica si un numero pertenece a la sucesión de Fibonacci.
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-2) + fibonacci (n-1)


esFibonacci :: Int -> Bool
esFibonacci n = existe' [1..n] (\x -> x == fibonacci n)

-- Ejemplo esFibonacci 5 = True, esFibonacci 6 = False

-- ► (i) todosFibonacci :: [Int] -> Bool, verifica si todos los numeros de una lista pertenecen a la sucesión de Fibonacci.
todosFibonacci :: [Int] -> Bool
todosFibonacci xs = paraTodo' xs esFibonacci

-- Ejemplo todosFibonacci [1,2,3] = True, todosFibonacci [1,2,3,4] = False



{-- 7
👉 Map es una función de orden superior que toma una función y una lista,
y aplica esa función a cada elemento de la lista, produciendo una nueva lista.
Ejemplo, map (*2) [1,2,3] produce la lista [2,4,6] que contiene los números de la
lista original multiplicados por 2.


map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

👉 Filter es una función de orden superior que toma una función booleana y una lista,
y devuelve la lista de todos los elementos que satisfacen la función booleana.
Ejemplo, filter even [1,2,3,4] produce la lista [2,4] que contiene los números pares.

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs) | f x = x : filter f xs
                | otherwise = filter f xs


succ es una función que toma un número y devuelve el siguiente.
succ n = n + 1
Ejemplo, succ 3 devuelve 4.

map succ [1, -4. 6, 2, -8]
[2, -3, 7, 3, -7]

filter esPositivo [1, -4. 6, 2, -8]
[1, 6, 2] retorna solo los positivos.

    -}

-- ► 8 Programá una función que dada una lista de números xs devuelve la lista de duplicar cada valor de xs

-- Recursiva
duplicar :: Num a => [a] -> [a]
duplicar [] = []
duplicar (x:xs) = x*2 : duplicar' xs

-- x*2 multiplica x por 2.
-- duplicar' xs aplica la funcion duplicar a la lista xs.

-- Ejemplo duplicar [1,2,3] = [2,4,6]

-- Con map
duplicar' :: Num a => [a] -> [a]
duplicar' xs = map (*2) xs

-- map (*2) xs multiplica cada elemento de xs por 2.

-- Ejemplo duplicar' [1,2,3] = [2,4,6]

-- ► 9 Programá una función que dada una lista de números xs, calcula una lista que tiene como elementos aquellos numeros de xs que son primos.

-- Recursiva
primos :: [Int] -> [Int]
primos [] = []
primos (x:xs) | esPrimo x = x : primos xs
              | otherwise = primos xs

-- x : primos xs agrega x a la lista primos xs.
-- primos xs aplica la funcion primos a la lista xs.

-- Ejemplo primos [1,2,3,4,5] = [2,3,5]

-- Con filter
primos' :: [Int] -> [Int]
primos' xs = filter esPrimo xs

-- Ejemplo primos' [1,2,3,4,5] = [2,3,5]

-- ► 10 La función primIguales toma un valor y una lista, y calcula el tramo inicial más largo de la lista cuyos elementos son todos iguales al valor dado.
-- Por ejemplo, primIguales 3 [3,3,3,4,1] devuelve [3,3,3].

{-- takeWhile es una función de orden superior que toma una función booleana y una lista,
y devuelve la lista de todos los elementos que satisfacen la función booleana.
Ejemplo, takeWhile even [2,4,6,7,8] produce la lista [2,4,6] que contiene los números pares.--}

-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- takeWhile p [] = []
-- takeWhile p (x:xs) | p x = x : takeWhile p xs
--                    | otherwise = []


-- Recursiva
primIguales :: Eq a => a -> [a] -> [a]
primIguales x [] = []
primIguales x (y:ys) | x == y = y : primIguales x ys
                     | otherwise = []

-- Con takeWhile
primIguales' :: Eq a => a -> [a] -> [a]
primIguales' x xs = takeWhile (==x) xs

-- takeWhile (==x) xs devuelve la lista de todos los elementos de xs que son iguales a x.



-- ► 11 La función primIguales' toma una lista y devuelve el tramo inicial más largo de la lista cuyos elementos son todos iguales.
-- Por ejemplo, primIguales' [3,3,3,4,1] devuelve [3,3,3].

-- Recursiva
primIguales'' :: Eq a => [a] -> [a]
primIguales'' [] = []
primIguales'' (x:xs) = x : primIguales'' (primIguales x xs)

-- Ejemplo primIguales'' [3,3,3,4,1] = [3,3,3]

-- Con takeWhile
primIguales''' :: Eq a => [a] -> [a]
primIguales''' xs = takeWhile (==head xs) xs

-- head xs devuelve el primer elemento de la lista xs (siempre y cuando no sea vacia), por ejemplo head [1,2,3] devuelve 1, head [] da error. 