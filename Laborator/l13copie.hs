{- Monada Maybe este definita in GHC.Base 

instance Monad Maybe where
  return = Just
  Just va  >>= k   = k va
  Nothing >>= _   = Nothing


instance Applicative Maybe where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor Maybe where              
  fmap f ma = pure f <*> ma   
-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

pos :: Int -> Bool
pos  x = if (x>=0) then True else False

fct :: Maybe Int ->  Maybe Bool
fct  mx =  mx  >>= (\x -> Just (pos x))

fct' :: Maybe Int ->  Maybe Bool
fct'  mx = do
    x <- mx
    return (pos x)

addM :: Maybe Int -> Maybe Int -> Maybe Int
addM mx my = case mx of
                Nothing -> Nothing
                Just x -> case my of
                            Nothing -> Nothing
                            Just y -> Just (x+y)

{-cartesian_product :: Num b => Maybe b -> Maybe b -> Maybe (b,b)
--cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))

cartesian_product' xs ys = do   
    x <- xs
    y <- ys
    return (x,y)
-}
--prod f xs ys = [f x y | x <- xs, y<-ys]

prod :: Num b => (b->b->b) -> [b] -> [b] -> [b]
prod f xs ys = [f x y |x <- xs, y<-ys]

prod' f xs ys = do 
    x <- xs
    y <- ys
    return (f x y)

myGetLine :: IO String
myGetLine = getChar >>= \x ->
      if x == '\n' then
          return []
      else
          myGetLine >>= \xs -> return (x:xs)

myGetLine' = do
    x <- getChar
    if x== '\n' then
        return []
    else do         --neaparat cu do, altfel eroare
        xs <- myGetLine
        return (x:xs)


prelNo noin =  sqrt noin

ioNumber = do
     noin  <- readLn :: IO Float
     putStrLn $ "Intrare\n" ++ (show noin)
     let  noout = prelNo noin
     putStrLn $ "Iesire"
     print noout
{-
ioNumber' =
    (readLn :: IO Float) >>= \noin -> 
    putStrin ("Intrare\n" ++ (show noin)) >> 
    let noout = prelNo noin in 
    putStrin "Iesire" >> 
    print noout
-}
data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN p = show "NAME: " ++ name p
showPersonA :: Person -> String
showPersonA p = show "AGE: " ++ show (age p)

{-
showPersonN $ Person "ada" 20
"NAME: ada"
showPersonA $ Person "ada" 20
"AGE: 20"
-}

showPerson :: Person -> String
showPerson p = "(" ++ showPersonN p ++ ", " ++ showPersonA p ++ ")"

{-
showPerson $ Person "ada" 20
"(NAME: ada, AGE: 20)"
-}


newtype Reader env a = Reader { runReader :: env -> a }

{-
instance Monad (Reader env) where
  return x = Reader (\_ -> x)
  ma >>= k = Reader f
    where f env = let a = runReader ma env
                  in  runReader (k a) env


instance Applicative (Reader env) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance Functor (Reader env) where              
  fmap f ma = pure f <*> ma    
-}
mshowPersonN ::  Reader Person String
mshowPersonN = undefined
mshowPersonA ::  Reader Person String
mshowPersonA = undefined 
mshowPerson ::  Reader Person String
mshowPerson = undefined 
{-
runReader mshowPersonN  $ Person "ada" 20
"NAME:ada"
runReader mshowPersonA  $ Person "ada" 20
"AGE:20"
runReader mshowPerson  $ Person "ada" 20
"(NAME:ada,AGE:20)"
-}

{-  instance Monad Maybe where
    mx >>= f = case mx of
                Nothing -> Nothing  
                Just x -> f x
    return = pure
-}




--partea 2

--- Monada Writer

newtype WriterS a = Writer { runWriter :: (a, String) } deriving Show


instance  Monad WriterS where
  return va = Writer (va, "")
  (>>=) :: WriterS a -> (a -> WriterS b) -> WriterS b
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance  Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterS where              
  fmap f ma = pure f <*> ma     

tell :: String -> WriterS () 
tell log = Writer ((), log)
  
logIncrement :: Int  -> WriterS Int
logIncrement x = do
    tell ("Increment " ++ show x ++ ", ")
    return (x + 1)

logIncrement2 :: Int  -> WriterS Int
logIncrement2 x = do
    y <- logIncrement x
    logIncrement y

logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x 0 = return x
logIncrementN x n = do
    y <- logIncrement x
    logIncrementN y (n-1)