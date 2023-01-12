

-- primeira questao 
elemNum :: Int -> [Int] -> Int
elemNum x [] = 0
elemNum y (x : xs) 
        | y == x = 1+ elemNum y xs
        |otherwise  =  elemNum y xs


-- segunda questao 
elemNum_2 :: Int -> [Int] -> Int
elemNum_2 z [] = 0 
elemNum_2 z y = length [ x | x <- y , x == z ]

-- terceira questao
tamanhoUnique :: [Int] -> [Int]-> [Int]
tamanhoUnique _ [] = []
tamanhoUnique y (x:xs) =  if  x `elemNum` y  == 1  
        then  [x] ++ tamanhoUnique y xs 
        else  case x `elemNum` y /= 1 of True-> tamanhoUnique y xs


unique:: [Int]->[Int]
unique y = tamanhoUnique y y  

 

-- quarta questao 
unique_2 :: [Int] -> [Int]
unique_2 [] = []
unique_2 y = [x | x <- y,elemNum_2 x y == 1 ]


-- quinta questao 
orderTriple :: (Int,Int,Int) -> (Int,Int,Int)
orderTriple (a, b ,c )
        | (a > c) && (c > b )  = (b,c,a)
        | (a > c) && (c < b ) && (a > b) = (c,b,a)
        | (a > b) && (c < b ) = (c,b,a)
        | (a > b) && (a > c) && (c > b ) = (b,c,a)
        | (c > a) && (a > b ) = (b,a,c)
        | (c > a) && (a < b )  && (c > a)= (a,b,c)
        | (c > b) && (a > b ) && ( c > a) = (b,a,c)
        | (c > b) && (a < b ) = (a,b,c)
        | (b > c) && (c > a ) = (a,c,b)
        | (b > c) && (c < a )  && (b > a)= (c,a,b)
        | (b > a) && (c > a ) && ( b > c) = (a,c,b)
        | otherwise  = (c,a,b)

