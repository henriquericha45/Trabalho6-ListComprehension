-- Henrique Levandoski Richa

{- 1.Usando List Comprehension escreva uma função, chamada divisoresden, que devolva uma
lista dos divisores de um número dado. -}
divisoresden :: Int -> [Int]
divisoresden n = [x | x <- [1..n], mod n x == 0]

{- 2. Usando  List Comprehension  escreva  uma  função,  chamada  contaCaractere,  que  conte  a 
ocorrência de um caractere específico, em uma string dada. -}
contaCaractere :: Char -> String -> Int
contaCaractere caractere palavra = length [x | x <- palavra, x == caractere]

{- 3. Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve 
o dobro dos valores dos elementos não negativos da lista de inteiros dada. -}
dobroNaoNegativo :: [Int] -> [Int]
dobroNaoNegativo list = [2*x | x <- list, x >= 0]

{- 4. Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista 
de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis de serem 
construídos por inteiros entre 1 e um número inteiro dado. -}
pitagoras :: Int -> [(Int, Int, Int)]
pitagoras n = [(a,b,c) | a <- [1..n], b<-[1..n], c<-[1..n], a^2 == b^2 + c^2]

{- 5. Números  perfeitos  são  aqueles  cuja  soma  dos  seus  divisores  é  igual  ao  próprio  número. 
Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva 
uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se 
que você já tem uma função que devolve uma lista dos divisores de um número dado. -}
numerosPerfeitos :: Int -> [Int]
numerosPerfeitos n = [x | x <- [1..n], x == sum (divisoresden x)]

{- 6. Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o 
produto escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e zip no 
prelude que podem ser úteis. -}
produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar l1 l2 = sum [fst x * snd x | x <- zip l1 l2]

{- 7. Usando  List Comprehension  escreva  uma  função,  chamada  primeirosPrimos,  que  devolva 
uma lista contendo os n primeiros números primos a partir do número 2. -}
primeirosPrimos :: Int -> [Int]
primeirosPrimos n = [x | x <- [2..n], ehPrimo x]

ehPrimo :: Int -> Bool
ehPrimo n = if (divisoresden n == [1,n]) then True else False

{- 8. Usando  List Comprehension  escreva  uma  função,  chamada  paresOrdenados,  que  devolva 
uma  lista  de  par  ordenados  contendo  uma  potência  de  2  e  uma  potência  de  3  até  um 
determinado número dado. Observe que estes números podem ser bem grandes. -}
paresOrdenados :: Int -> [(Int, Int)]
paresOrdenados n = [(x, y) | x <- [2^y | y <- [0..n]], y <- [0..n]]


main = do

    putStr "Func. 1: entrada 17, resultado: "
    print (divisoresden 17)
    
    putStr "Func. 2: entrada e henrique, resultado: "
    print (contaCaractere 'e' "henrique")

    putStr "Func. 3: entrada [-1, 2, 3], resultado: "
    print (dobroNaoNegativo [1, -2, 3, 4, 5])
    
    putStr "Func. 4: entrada 15, resultado: "
    print (pitagoras 15)
    
    putStr "Func. 5: entrada 15, resultado: "
    print (numerosPerfeitos 15)

    putStr "Func. 6: entrada [1,2,3] [4,5,6], resultado: "
    print (produtoEscalar [1,2,3] [4,5,6])

    putStr "Func. 7: entrada 15, resultado: "
    print (primeirosPrimos 15)

    putStr "Func. 8: entrada 15, resultado: "
    print (paresOrdenados 7)
