
-- primeira questão 


twice :: (Int -> Int)-> Int -> Int 
twice f x = f (f x)

double :: Int -> Int 
double x = x *2 

power :: Int -> Int 
power x = x ^2 

-- segunda questão 

filterFirst :: (Int -> Bool) -> [Int] -> [Int]
filterFirst _ [] = []
filterFirst f (x: xs) = 
        if  f x == False  then  xs 
        else  x: filterFirst f xs