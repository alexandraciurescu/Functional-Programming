import Data.List
{-  1. ??
    2. (1,1), (1,2), (1,3) samd
    3. (1,1) (2,1) (2,2) (2,3) (2,4)
       (3,...)
    4. FMI
    5. [1]
-}

factori :: Int -> [Int]
factori n = [d | d <- [1..n], n `mod` d == 0]

prim :: Int -> Bool
prim n 
  | length (factori n) ==2 = True
  | otherwise = False

numerePrime :: Int -> [Int]
numerePrime n = [x | x<-[2..n], prim(x)==True]

{-myzip3 :: [Int] -> [Int] ->[Int] ->[Int]
myzip3 l1 l2 l3 = [ (x,y,z) | x <- l1 ] -}

myzip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
myzip3 [] _ _ = []  -- Dacă prima listă este goală, rezultatul este lista goală
myzip3 _ [] _ = []  -- Dacă a doua listă este goală, rezultatul este lista goală
myzip3 _ _ [] = []  -- Dacă a treia listă este goală, rezultatul este lista goală
myzip3 (x:xs) (y:ys) (z:zs) = (x, y, z) : myzip3 xs ys zs

primul :: (Char, Int) -> Char
primul (s,i) = s

firstEl :: [(Char,Int)] -> String
firstEl lista = map primul lista



sumList :: [[Int]] -> [Int]
sumList ls = map (sum) ls


f :: Int -> Int
f n
  | odd n = 2*n
  | otherwise = div n 2


prel2 :: [Int] -> [Int]
prel2 ls = map (f) ls


fccar :: Char -> [String] -> [String]
fccar c ls = filter (elem c) ls

ridicarepatrat :: Int -> Int
ridicarepatrat x = x * x

patrateimpare :: [Int] ->[Int]
patrateimpare ls = map ridicarepatrat (filter (odd) ls )

aux :: (Int,Int) ->Int
aux (a,b) = b*b

patratePozitiiImpare :: [Int] -> [Int]
patratePozitiiImpare xs = map aux pozitiiImpare
    where
        pozitiiImpare = [(x,y) | (x, y) <- zip [1..] xs, odd x]

consoana :: Char -> Bool
consoana c
  | c `elem` "aeiouAEIOU" = True
  | otherwise = False

elimconsoane :: String -> String
elimconsoane s = filter consoana s

numaiVocale :: [String] -> [String]
numaiVocale ls = map elimconsoane ls

