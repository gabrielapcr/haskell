converteABC :: Char -> Char
converteABC x
        | fromEnum x < 97 || fromEnum x > 122 = x
        | otherwise = toEnum(fromEnum x - 32)


romanDigit :: Int -> String
romanDigit x
        | x == 0 = "0"
        | x == 1 = "I"
        | x == 2 = "II"
        | x == 3 = "III"
        | x == 4 = "IV"
        | x == 5 = "V"
        | x == 6 = "VI"
        | x == 7 = "VII"
        | x == 8 = "VIII"
        | x == 9 = "XIX"
        | x == 10 = "X"
        | otherwise = "INDISPONIVEL"


digits :: String -> String
digits [] = []
digits (x:xs)
        | isNumber x "0123456789" = x:digits xs
        | otherwise = digits xs


newReverse [] = []
newReverse (x:xs) = newReverse xs ++ [x]