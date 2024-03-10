import Control.Monad (guard)

data Point = Pt [Int]
 deriving Show

data Arb = Empty | Node Int Arb Arb
 deriving Show
 
class ToFromArb a where
 toArb :: a -> Arb
 fromArb :: Arb -> a


instance ToFromArb Point where
    toArb (Pt []) = Empty
    toArb (Pt (x:xs)) = Node x (toArb (Pt (filter (< x) xs))) (toArb (Pt (filter (>= x) xs)))

    fromArb Empty = Pt []
    fromArb (Node x left right) = Pt (fromArbToList left ++ [x] ++ fromArbToList right) where
        fromArbToList Empty = []
        fromArbToList (Node x left right) = fromArbToList left ++ [x] ++ fromArbToList right

--2
getfromInterval :: Int -> Int -> [Int] -> [Int]
getfromInterval _ _ [] = []
getfromInterval a b (h:t) 
   | h<=b && h>=a = [h] ++ getfromInterval a b t
   | otherwise = getfromInterval a b t


getfromInterval2 :: Int -> Int -> [Int] -> Maybe [Int]
getfromInterval2 a b [] = Just []
getfromInterval2 a b (h:t) =
    do 
        if (h<=b && h>=a) then do
         rest <- getfromInterval2 a b t 
         return (h : rest)
        else do getfromInterval2 a b t

        
filter_range_monade_do :: Int -> Int -> [Int] -> [Int]
filter_range_monade_do a b xs = 
    do
     x <- xs
     if x >= a && x <= b then return x else []


{-
getFromIntervalM :: Int -> Int -> [Int] -> Maybe [Int]
getFromIntervalM _ _ [] = Just []  -- Dacă lista este goală, întoarce o listă goală în monada Maybe
getFromIntervalM a b (h:t) = do
    guard (h >= a && h <= b)  -- Verifică dacă elementul este în intervalul specificat
    restul <- getFromIntervalM a b t  -- Continuă să parcurgi lista recursiv
    return (h:restul)  -- Returnează elementul curent împreună cu restul listei
    -}