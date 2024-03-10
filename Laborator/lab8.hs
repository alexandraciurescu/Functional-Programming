import Prelude hiding (lookup)
import Data.List (intercalate)
import GHC.Windows (LPTSTR)

class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert :: Ord key
      => key -> value -> c key value -> c key value
  lookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  values :: c key value -> [value]
  toList :: c key value -> [(key, value)]
  fromList :: Ord key => [(key,value)] -> c key value

  keys = map fst . toList  --luam primul elem din pereche
  values = map snd . toList --luam al doilea elem din pereche
  fromList = foldr (\(k,v) acc -> insert k v acc) empty

newtype PairList k v
  = PairList { getPairList :: [(k, v)] }


instance Collection PairList where
    empty = PairList []
  
    
    singleton k v = PairList [(k, v)]
  
    insert k v (PairList pairs) = PairList $ [(k, v)] ++ pairs
  
    lookup k (PairList ps) = lookup' k ps
        where
            lookup' _ [] = Nothing
            lookup' key ((k', v'):rest)
                | key == k' = Just v'
                | otherwise = lookup' key rest

    delete k (PairList pairs) = PairList $ filter (\(key, _) -> key /= k) pairs
  
    keys (PairList pairs) = map fst pairs
  
    values (PairList pairs) = map snd pairs
  
    toList (PairList pairs) = pairs
  
    fromList pairs = PairList pairs 



data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare

instance Collection SearchTree where
    empty = Empty
 
    singleton k v = BNode Empty k (Just v) Empty
    
    insert k v Empty = singleton k v
    insert k v (BNode left key value right)
     | k<key = BNode (insert k v left) key value right
     | k>key = BNode left key value (insert k v right)
     | otherwise = BNode left key (Just v) right

    lookup _ Empty = Nothing
    lookup k (BNode left key value right)
       | k<key = lookup k left
       | k>key = lookup k right
       | otherwise = value

    delete _ Empty = Empty
    delete k (BNode left key value right)
     | k<key = delete k left
     | k>key = delete k right
     | otherwise = BNode left key Nothing right

    keys Empty = []
    keys (BNode left key _ right) = keys left ++ [key] ++ keys right

    values Empty = []
    values (BNode left _ (Just value) right) = values left ++  [value] ++ values right
    values (BNode left _ Nothing right) = values left ++ values right

    toList Empty = []
    toList (BNode left key (Just value) right) = toList left ++ [(key, value)] ++ toList right
  
    fromList [] = Empty
    fromList ((k,v):xs) = insert k v (fromList xs)



data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
    deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

{-Dacă lista de coordonate nu este goală, folosim unwords (map show coords) pentru a transforma lista de 
coordonate într-un șir de caractere format din coordonate separate prin spații. map show coords transformă 
fiecare coordonată într-un șir de caractere. unwords concatenează aceste șiruri de caractere, 
separându-le prin spații.-}

instance Show Punct where
  show (Pt []) = "()"
  show (Pt coords) = "(" ++ intercalate ", " (map show coords) ++ ")"
   

{-instance ToFromArb Punct where
    toArb (Pt [])  = Vid
    toArb (Pt (x:xs)) = N (F x) (toArb' xs)
         where
            toArb' [] = Vid
            toArb' (y:ys) = N (F y) (toArb' ys)
    
    fromArb Vid = Pt []
    fromArb (F f) = Pt [f]
    fromArb (N left right) = Pt (x : coords)
        where
         Pt coords = fromArb (N left Vid right) -}

instance ToFromArb Punct where
    toArb (Pt []) = Vid
    toArb (Pt (x:xs)) = insertIntoTree x xs
        where
            insertIntoTree :: Int -> [Int] -> Arb
            insertIntoTree x [] = F x
            insertIntoTree x (y:ys) = N (F x) (insertIntoTree y ys)

    fromArb Vid = Pt []
    fromArb (F x) = Pt [x]
    fromArb (N left right) = Pt (frontieraArb left ++ frontieraArb right)
        where
            frontieraArb :: Arb -> [Int]
            frontieraArb Vid = []
            frontieraArb (F x) = [x]
            frontieraArb (N l r) = frontieraArb l ++ frontieraArb r




data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) =>  g a -> a

-- ghci> pi
-- 3.141592653589793

instance GeoOps Geo where
    perimeter (Square l) = 4*l
    perimeter (Rectangle l b)  = 2*l + 2*b
    perimeter (Circle r) = 2*pi*r

    area (Square l) =l*l
    area (Rectangle l b)  = l *b
    area (Circle r) = pi*r*r

instance (Floating a, Eq a) => Eq (Geo a) where
  g1 == g2 = perimeter g1 == perimeter g2

