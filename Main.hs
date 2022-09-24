--RAFAEL BAUER SAMPAIO

--1. Usando List Comprehension escreva uma função, chamada divisoresDeN, que devolva uma lista dos divisores de um número dado.
divisoresDeN :: Int -> [Int]
divisoresDeN n = [x | x <-[1..n], mod n x == 0]

--2. Usando List Comprehension escreva uma função, chamada contaCaractere, que conte a ocorrência de um caractere específico, em uma string dada.
contaCaractere :: String -> Char -> Int
contaCaractere palavra c =  length $ filter (== c) palavra


--3. Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve o dobro dos valores dos elementos não negativos da lista de inteiros dada.
dobroNaoNegativo :: [Int] -> [Int]
dobroNaoNegativo lista = [x*2 | x <- lista, x>=0]

--4. Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis de serem construídos por inteiros entre 1 e um número inteiro dado.
pitagoras :: Int -> [(Int, Int, Int)]
pitagoras x = [(c1,c2,h) | c1 <- [1..x], c2 <- [1..x], h <- [1..x], h^2 == c1^2 + c2^2]

--5. Números perfeitos são aqueles cuja soma dos seus divisores é igual ao próprio número. Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se que você já tem uma função que devolve uma lista dos divisores de um número dado.
numerosPerfeitos :: Int -> [Int]
numerosPerfeitos n = [x | x <- [1..n], ((sum (divisoresDeN x) - x) == x)]

--6. Usando List Comprehension escreva uma função, chamada produtoEsc9alar, que devolva o produto escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e zip no prelude que podem ser úteis.
produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar lista1 lista2 = sum [x*y | (x,y) <- zip lista1 lista2]

--7. Usando List Comprehension escreva uma função, chamada primeirosPrimos, que devolva uma lista contendo os n primeiros números primos a partir do número 2.
ePrimo :: Int -> Bool
ePrimo x = divisoresDeN x == [1, x] 

primeirosPrimos :: Int -> [Int]
primeirosPrimos n = take n [x | x <- [2..], ePrimo x]

--8. Usandi List Comprehension escreva uma função, chamada paresOrdenados, que devolva uma lista de par ordenados contendo uma potência de 2 e uma potência de 3 até um determinado número dado. Observe que estes números podem ser bem grandes.
paresOrdenados :: Int -> [(Int, Int)]
paresOrdenados n = [(2^x,3^x) | x <-[0..n]]


main = do
  print("1. Divisores de n, entrada: 54, saida: ", divisoresDeN 54)
  print("2. Conta caracter, entrada: 'Isso e um teste' 'e', saida: ", contaCaractere "Isso e um teste" 'e')
  print("3. Dobro nao negativo, entrada: [-3,4,2,-1], saida: ", dobroNaoNegativo [-3,4,2,-1])
  print("4. Pitagoras, entrada: 25, saida: ", pitagoras 25)
  print("5. Numeros perfeitos, entrada: 1000, saida: ", numerosPerfeitos 1000)
  print("6. Produto escalar, entrada: [1,2,3,4,5] [5,4,3,2,1], saida: ", produtoEscalar [1,2,3,4,5] [5,4,3,2,1])
  print("7. Primeiros primos, entrada: 8, saida: ", primeirosPrimos 8)
  print("8. Pares ordenados, entrada: 10, saida: ", paresOrdenados 10)
