

type Pair = (Int, Int)

-- funções suportes para a realização da função principal 


-- função para retornar quantas vezes um número apareceu em uma lista 
elemNum :: Int -> [Int] -> Int
elemNum x [] = 0
elemNum y (x : xs) 
        | y == x = 1+ elemNum y xs
        |otherwise  =  elemNum y xs


-- função para verificar se um número esta na lista 
dentro :: Int -> [Int] -> Bool
dentro y [] = False
dentro y (x:xs) = if  y == x then  True 
                 else  if  x /= y  then dentro y xs


-- funções para colocar os números das demais listas em uma única 
paraDefinir :: [Int] -> [Int] -> [Int]
paraDefinir y [] = y
paraDefinir y (x:xs) 
    | x `dentro` y  = paraDefinir y xs
    |otherwise  paraDefinir (y ++ [x]) xs


definir :: [Int] -> [Int]
definir n = paraDefinir [] n


-- função para contar quantas vezes o número apareceu na lista 
tudo :: [[Int]] -> [Int]
tudo [] = []
tudo [x] = [x]
tudo (x:xs) = x ++ tudo xs


-- função para passar para tuplas 
tuplas :: [Int] -> [Int] -> [Pair]
tuplas _ [] = []
tuplas y (x:xs) = [(x, x `elemNum` y)] ++ tuplas y xs


-- função principal 
group :: [[Int]] -> [(Int, Int)]
group [] = []
group y = tuplas listao (set listao)
    where
        listao = todos lst



