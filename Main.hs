-- Aluno: Alan Renato Bunese
-- Disciplina: Programação Funcional
-- Professor: Frank Alcantara
module Main (main) where

import Data.Char (digitToInt)

{- 1.
 - Utilizando a linguagem Haskell e o seu próprio tipo de dados,
 - crie um programa capaz de classificar triângulos a partir do comprimento
 - dos seus lados sabendo que o comprimento de cada um dos lados deve ser
 - maior que zero e que:
 - - a. triângulos equiláteros têm todos os lados do mesmo tamanho;
 - - b. triângulos esosceles têm, no mínimo, dois lados do mesmo tamanho;
 - - c. triângulos escalenos têm todos os lados de tamanhos diferentes;
 - - d. triângylos degenerados têm um lado igual a soma dos outros dois e área zero.
 -}
data TipoTriangulo = Equilatero | Isosceles | Escaleno | Degenerado | NaoTriangulo
    deriving (Show, Eq)

classificaTriangulo :: Float -> Float -> Float -> TipoTriangulo
classificaTriangulo a b c
    | a <= 0 || b <= 0 || c <= 0 = NaoTriangulo
    | a == b && b == c = Equilatero
    | a == b || b == c || a == c = Isosceles
    | a == b + c || b == a + c || c == a + b = Degenerado
    | a /= b && b /= c && a /= c = Escaleno 
    | otherwise = NaoTriangulo

{- 2.
 - Usando Haskell, crie uma função chamada fatias, com a assinatura dada por fatias :: Int -> String -> [[Int]],
 - que receba uma string e um inteiro e devolva uma lista de listas contendo em cada item uma lista de inteiros.
 - Essa função receberá strings contendo digitos como , por exemplo: "345234678" e devolverá listas parecidas
 - com [[3, 4, 5], [4, 5, 2], [5, 2, 3], [2, 3, 4], [3, 4, 6], [4, 6, 7], [6, 7, 8]]
 - No caso do exemplo, o inteiro que fatias recebeu foi 3. Observe que você poderá criar, quantas funções de
 - apoio acredite que sejam necessárias para criar as funcionalidades de fatias inclusive, se achar interessante,
 - pode usar as funções mapMaybe e digitToInt.
 -}

fatias :: Int -> String -> [[Int]]
fatias n s = map (map digitToInt) (fatias' n s)

fatias' :: Int -> String -> [String]
fatias' n s
    | length s < n = []
    | otherwise = take n s : fatias' n (tail s)

{- 3.
 - Usando Haskell escreva uma função chamada romanos que receba um inteiro menor ou
 - igual a 3000 e devolva uma string deste inteiro representado com algarismos romanos.
 -}
romanos :: Int -> String
romanos n
    | n <= 0 = ""
    | n > 3000 = "Número muito grande!"
    | n >= 1000 = "M" ++ romanos (n - 1000)
    | n >= 900 = "CM" ++ romanos (n - 900)
    | n >= 500 = "D" ++ romanos (n - 500)
    | n >= 400 = "CD" ++ romanos (n - 400)
    | n >= 100 = "C" ++ romanos (n - 100)
    | n >= 90 = "XC" ++ romanos (n - 90)
    | n >= 50 = "L" ++ romanos (n - 50)
    | n >= 40 = "XL" ++ romanos (n - 40)
    | n >= 10 = "X" ++ romanos (n - 10)
    | n >= 9 = "IX" ++ romanos (n - 9)
    | n >= 5 = "V" ++ romanos (n - 5)
    | n >= 4 = "IV" ++ romanos (n - 4)
    | n >= 1 = "I" ++ romanos (n - 1)
    | otherwise = ""

{- 4.
 - Usando linguagem Haskell, escreva uma função que recebe uma lista de listas de inteiros
 - com até 5 digitos em cada lista e devolva apenas as listas que contenham palíndromes
 - primos. Por exemplo na lista de listas [[1, 1, 1], [1, 2, 2,], [3, 1, 3], [3, 1, 5]]
 - apenas o elemento [3, 1, 3] [e um palíndrome primo. Uma ferramente importante para criar
 - suas listas de teste pode ser encontrada em: Prime Number Calculator (calculatorsoup.com).
 -}
palindromo :: Int -> Bool
palindromo n = show n == reverse (show n)

primo :: Int -> Bool
primo n = length [x | x <- [1..n], n `mod` x == 0] == 2

palindromoPrimo :: [[Int]] -> [[Int]]
palindromoPrimo = filter (\x -> primo (read (concatMap show x)) && palindromo (read (concatMap show x)))

{- 5.
 - Usando a linguagem Haskell escreva uma função, chamada ultimoNome que receba o nome
 - completo de uma pessoa e devolva apenas o último sobrenome sem qualquer vogal. Caso
 - o ultimo sobrenome não contenha nenhuma vogal devolva o ultimo sobrenome que ainda
 - contenha vogal. Por exemplo se o nome for "Ana Maria stzrx", a função deve devolver Maria,
 - se o nome for Silvia Silva a função deve devolver Slv.

 - Acredito que esta função talvez esteja com alguma explicação incorreta.
 - Então vou assumir que o que se quer é o último sobrenome que contenha vogal,
 - mas que retorne este nome sem qualquer vogais.
 -}
ultimoNome :: String -> String
ultimoNome s = filter (\x -> not (elem x "aeiouAEIOU")) (last (filter (\x -> (any (\y -> elem y "aeiouAEIOU") x)) (words s)))

-- Main...
main :: IO()
main = do
    -- Testes...
    print ("classificaTriangulo: entrada: 1 1 1; resultado: " ++ show (classificaTriangulo 1 1 1))
    print ("classificaTriangulo: entrada: 1 1 2; resultado: " ++ show (classificaTriangulo 1 1 2))
    print ("classificaTriangulo: entrada: 1 2 4; resultado: " ++ show (classificaTriangulo 1 2 4))
    print ("classificaTriangulo: entrada: 1 1 3; resultado: " ++ show (classificaTriangulo 1 1 3))
    print ("classificaTriangulo: entrada: 5 2 3; resultado: " ++ show (classificaTriangulo 5 2 3))
    print ("classificaTriangulo: entrada: 0 2 3; resultado: " ++ show (classificaTriangulo 0 2 3))

    print ("fatias: entrada: 3 \"345234678\"; resultado: " ++ show (fatias 3 "345234678"))
    print ("fatias: entrada: 5 \"123456789\"; resultado: " ++ show (fatias 5 "123456789"))

    print ("romanos: entrada: 1; resultado: " ++ show (romanos 1))
    print ("romanos: entrada: 750; resultado: " ++ show (romanos 750))
    print ("romanos: entrada: 2999; resultado: " ++ show (romanos 2999))

    print ("palindromoPrimo: entrada: [[1, 1, 1], [1, 2, 2], [3, 1, 3], [3, 1, 5]]; resultado: " ++ show (palindromoPrimo [[1, 1, 1], [1, 2, 2], [3, 1, 3], [3, 1, 5]]))

    print ("ultimoNome: entrada: \"Ana Maria stzrx\"; resultado: " ++ show (ultimoNome "Ana Maria stzrx"))
    print ("ultimoNome: entrada: \"Silvia Silva\"; resultado: " ++ show (ultimoNome "Silvia Silva"))
