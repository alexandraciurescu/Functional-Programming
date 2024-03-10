import Data.List

{-1-}
sump :: [Int] ->Int
sump ls = foldr (+) 0 (map (^2) (filter odd ls))

{-2-}
verif :: [Bool] ->Bool
verif ls = foldr (&&) True ls

{-3-}
allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies f ls = (ls==filter f ls)

{-4-}
anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies f ls = (filter f ls /= [])

{-5
mapFoldr :: (Int->Int) -> [Int] ->[Int]
mapFoldr f ls = foldr (f) [] ls
-}


listToInt :: [Integer] -> Integer
listToInt xs = foldl (\acc x -> acc * 10 + x) 0 xs


rmChar :: Char -> String -> String
rmChar c s = filter (/=c) s

rmCharsRec :: String -> String -> String
rmCharsRec "" s1 = s1
rmCharsRec (h:t) s1 =  rmCharsRec t (rmChar h s1)

rmCharsFold :: String -> String -> String
rmCharsFold chars str = foldr (\x acc -> rmChar x acc) str chars

myReverse :: [Int] -> [Int]
myReverse ls = foldr (\x acc -> acc ++ [x]) [] ls

myElem :: Int -> [Int] -> Bool
myElem a ls = foldl (\ acc x -> acc || (x==a) ) False ls


myUnzip :: [(Char, Char)] -> ([Char],[Char])
myUnzip ls = foldr (\(x, y) (as, bs) -> (x:as, y:bs)) ([], []) ls


union :: [Int] -> [Int] -> [Int]
union xs ys = foldr (\x acc -> if x `elem` acc then acc else x : acc) ys xs

intersectt :: [Int] -> [Int] -> [Int]
intersectt xs ys = foldr (\x acc -> if x `elem` ys then x : acc else acc) [] xs

permutations :: [Int] -> [[Int]]
permutations [] = [[]]  -- Cazul de bază: lista vidă are o singură permutare, lista vidă
permutations (x:xs) = foldr (\ys acc -> foldr (\i acc' -> insertAt i ys acc') acc [0..length ys]) [] (permutations xs)
  where
    insertAt :: Int -> a -> [a] -> [a]
    insertAt n x xs = let (ys, zs) = splitAt n xs in ys ++ [x] ++ zs