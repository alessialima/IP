--Guía 4 de algoritmo y estructura de datos 
--1
fibo :: Int -> Int 
fibo n | n == 0 = 0 
       | n == 1 = 1 
       | n > 1 = (n-1) + (n-2)
--2
parteEnt :: Float -> Float
parteEnt x | 0 >= x && x < 1 = 0 
           | otherwise = 1 + parteEnt (x-1)
--3
esDiv :: Int -> Int -> Bool 
esDiv a b | a < b = False
          | a == b = True 
          | mod a b == 0 = True
          | otherwise = False 
--4
sumaImpares :: Int -> Int 
sumaImpares a | a == 1 = 1
              | a == 0 = 0 
              | a > 1 = imparesAnteriores a 

imparesAnteriores :: Int -> Int 
imparesAnteriores a | mod a 2 == 0 = ((a - 1) - (div (a - 1) 2)) ^ 2
                    | mod a 2 == 1 = (a - (div a 2)) ^ 2

--5 probar si funciona !!
medioFact :: Int -> Int
medioFact a | a == 0 = 1 
            | a == 1 = 1
            | otherwise = a * (medioFact (a-2))

--6 esta mal pq solo dice que es verdad si por lo menos 1 se repite. cambiar 
digitosIguales :: Int -> Bool 
digitosIguales n | n < 10 = True 
                 | (n < 100) && (mod n 10 == div n 10) = True 
                 | (n > 100) = digitosIguales (mod n 10)
                 | otherwise = False
--7 hecho en clase 
iesimoDigito :: Int -> Int -> Int 
iesimoDigito n 1 = div n (10^((cantDigitos n)-1))
iesimoDigito n i = iesimoDigito sacarPrimero (i-1)
  where sacarPrimero = mod n (10^((cantDigitos n)-1))

cantDigitos :: Int -> Int 
cantDigitos n | n < 10 = 1 
              | otherwise = 1 + cantDigitos (div n 10)

--8
sumaDigitos :: Integer -> Integer 
sumaDigitos a | a < 10 = a 
              | otherwise = (mod a 10) + (sumaDigitos (div a 10))
 
--9 hacer

--10 a
sumatoria :: Int -> Int 
sumatoria n | n == 0 = 1
            | n == 1 = 3
            | otherwise = (2^n) + ((2^n)-1)

--11
eAprox :: Float -> Float
eAprox n | n == 0 = 1
         | otherwise = (1 / (factorial n)) + eAprox (n - 1)

factorial :: Float -> Float
factorial n | n == 1 = 1
            | n > 1 = n * (factorial (n - 1))

--b) creo el numero e 
e :: Float 
e = eAprox (10)

--12
raiz2 :: Float -> Float 
raiz2 n | n == 1 = 1
        | otherwise = (serie n) - 1

serie :: Float -> Float 
serie n | n == 1 = 2
        | otherwise = 2 + (1 / serie (n - 1))
