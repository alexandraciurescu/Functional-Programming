import Data.List
import Data.Char

ePalindrom :: String -> Bool
ePalindrom s = s==(reverse s)

eVocala :: Char -> Bool
eVocala c= elem c "aeiouAEIOU"

numaraVocale :: String -> Int
numaraVocale "" =0
numaraVocale(c:s)
   |eVocala c = 1 + numaraVocale s
   |otherwise = numaraVocale s

nrVocale :: [String] -> Int
nrVocale(s:xs)
    |ePalindrom s = numaraVocale s + nrVocale xs
    |otherwise = nrVocale xs



addLista :: Int -> [Int] ->[Int]
addLista n [] = []
addLista n (h:t)
  | even h = [h] ++ [n] ++ addLista n t
  | otherwise = [h] ++ addLista n t


divizori :: Int -> [Int]
divizori n = [d | d<-[1..n], mod n d==0]

listadiv :: [Int] -> [[Int]]
listadiv lista = [[d | d<-[1..n], mod n d==0] | n<-lista]

inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec x y [] = []
inIntervalRec x y (h:t)
   |h `elem` [x..y] = [h] ++ inIntervalRec x y t
   |otherwise = inIntervalRec x y t

inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp x y ls= [n | n <- ls, n>=x, n<=y]

pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (h:t)
  | h>0 = 1+pozitiveRec t
  | otherwise = pozitiveRec t

{-pozitiveComp :: [Int] -> Int
pozitiveComp ls = length [n| n<-ls, n>0] -}



{-case gasestePozitia numar lista of
        Just pozitie -> putStrLn ("Numărul se află la poziția " ++ show pozitie)
        Nothing      -> putStrLn "Numărul nu se găsește în listă."

pozimparerec :: [Int] -> [Int]
pozimparerec [] = []
pozimparerec (h:t)
   |even h = pozimparerec t
   |otherwise = [pozitieh h (h:t)] ++ pozimparerec t
-}

pozitiiImpare :: [Int] -> [Int]
pozitiiImpare xs = go xs 0
  where
    go [] _ = []  -- Cazul de bază: lista vidă
    go (x:xs) i
      | odd x = i : go xs (i + 1)  -- Dacă elementul este impar, adăugăm indicele la rezultat
      | otherwise = go xs (i + 1)  -- Altfel, continuăm parcurgerea fără a adăuga indicele


multDigits :: String -> Int
multDigits "" = 1
multDigits s
   |isDigit (s !! 0) = digitToInt (s !! 0) * multDigits (drop 1 s)
   |otherwise = multDigits (drop 1 s)


multDigitsComp :: String -> Int
multDigitsComp s =product [digitToInt c | c<-s, isDigit c==True]

