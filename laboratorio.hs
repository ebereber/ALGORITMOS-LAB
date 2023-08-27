
-- 1 Programá las siguientes funciones
-- ► (a)
esCero x = x == 0 
-- ► (b)
esPositivo x = x > 0
-- ► (c)

esVocal x | x == 'a' = True
          | x == 'e' = True
          | x == 'i' = True
          | x == 'o' = True
          | x == 'u' = True
          | otherwise = False

esVocal2 x = x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u'

-- ► (d)
valorAbsoluto x | x >= 0 = x
                | otherwise = -x

-- 2 Programá la siguiente funciones usando recursión o composición
-- ► (a)
paraTodo [] = True
paraTodo (x:xs) = x && paraTodo xs 

-- ► (b)
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs


-- ► (c)
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- ► (d)
factorial 0 = 1
factorial n = n * factorial (n-1)

-- ► (e)
promedio xs = sumatoria xs / fromIntegral (length xs)

-- 3 Programá la función pertenece :: Int -> [Int] -> Bool, que verifica si  un numero se encuentra en una lista.
pertenece x [] = False
pertenece x (y:ys) = x == y || pertenece x ys

-- 4 Programá las funciones que implementan los cuantificadores generales. Notá que el segundo parámetro de cada función, es otra función. 

-- ► (a) 
paraTodo' :: [a] -> (a -> Bool) -> Bool
paraTodo' [] f = True
paraTodo' (x:xs) f = f x && paraTodo' xs f

-- ► (b)
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] f = False
existe' (x:xs) f = f x || existe' xs f

-- ► (c)
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] f = 0
sumatoria' (x:xs) f = f x + sumatoria' xs f

-- ► (d)
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] f = 1
productoria' (x:xs) f = f x * productoria' xs f

--5 Definí la función paratodo, pero usando la función paraTodo' sin recursión ni análisis por casos.
paraTodo'' xs = paraTodo' xs id

-- 6
