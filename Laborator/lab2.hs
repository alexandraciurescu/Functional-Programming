{-poly :: Double -> Double -> Double -> Double -> Double
poly a b c x = a*x*x+b*x+c

eeny :: Integer -> String
eeny a = if(even(a)==True) then "eeny" else "meeny"


{-fizzbuzz :: Integer -> String
fizzbuzz x = if((mod x 3)==0 && (mod x 5)==0)
             then "FizzBuzz"
             else if ((mod x 3)==0 && (mod x 5)==1)
                   then "Fizz"
                   else if ((mod x 3)==1 && (mod x 5)==0)
                         then "Buzz"
                         -}


                         
 {-tribonacci1 :: Integer -> Integer
 tribonacci1 n 
  | n < 3 = n
  | n == 3 = n-1
  | otherwise = tribonacci1(n-1)+tribonacci1(n-2)+tribonacci1(n-3)
  -}


{-tribonacci2 :: Integer -> Integer
tribonacci2 1 = 1
tribonacci2 2 = 1
tribonacci2 3 = 2
tribonacci2 n = tribonacci2(n-1)+tribonacci2(n-2)+tribonacci2(n-3)-}

{-binomial :: Integer -> Integer -> Integer
binomial n k
    |n=0       = 0
    |k=0       = 1
    |otherwise = binomial(n-1,k)+binomial(n-1,k-1)
-}

verifL :: [Int] -> Bool
verifL lista =
    if(even (length lista)==True) then True
    else False


takefinal :: [Int] -> Int ->[Int]
takefinal lista n =
    if(n > length lista) then lista
    else drop ((length lista) - n) lista

remove :: [Int] -> Int -> [Int]
remove lista n =
    (take (n-1) lista) ++ (drop (n) lista)
        

myreplicate :: Int -> Int -> [Int]
myreplicate 0 v = []
myreplicate n v = [v] ++ myreplicate (n-1) v

sumImp :: [Int] -> Int
sumImp []=0
sumImp (h:t) = 
    if(even h) then sumImp t
    else h + sumImp t

totalLen :: [String] -> Int
totalLen []=0
totalLen (h:t) =
    if((head h) =='A') then (length h) + totalLen(t)
    else totalLen(t)

-}

{-foo1 :: (Int,Char,String) -> String 
foo1 (a,s,st) = "aa"

foo2 :: (Int, (Char,String)) -> String
foo2 (a,(c,chr))="miumiu"

foo3 :: Int -> Char -> String
foo3 a str = "as"


compose :: [ a -> a ] -> ( a -> a )
compose f = foldr ( . ) id f

rev :: [a] -> [a]
rev = foldl (flip (:)) [] [1..3]
-}


