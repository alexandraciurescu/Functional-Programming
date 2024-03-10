pos :: Int -> Bool
pos x = if (x>=0) then True else False

fct :: Maybe Int -> Maybe Bool
fct mx = mx >>= (\x -> Just (pos x))

fctt :: Maybe Int -> Maybe Bool
fctt mx = do 
    x <- mx
    Just (pos x)

--2
addM :: Maybe Int -> Maybe Int -> Maybe Int
addM _ Nothing = Nothing
addM Nothing _ = Nothing
addM (Just x) (Just y) = Just (x+y)

addM1 :: Maybe Int -> Maybe Int -> Maybe Int
addM1 _ Nothing = Nothing
addM1 Nothing _ = Nothing
addM1 (Just x) (Just y) = do
    Just (x+y)


--cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))

cartesian_product :: Num b => Maybe b -> Maybe b -> Maybe (b,b)
cartesian_product xs ys = do
    x <- xs 
    y <- ys
    return (x,y)

f :: Int -> Int -> Int
f a b = a+b


--prod f xs ys = [f x y | x <- xs, y<-ys]
prod :: Num b => (b->b->b) -> [b] -> [b] -> [b]
prod f xs ys = do 
    x <- xs
    y <- ys
    return (f x y)



myGetLine :: IO String
myGetLine = getChar >>= \x ->
   if x == '\n' then
     return []
   else
    myGetLine >>= \xs -> return (x:xs)

myGetLine1 :: IO String
myGetLine1 = do
    x <- getChar
    if x == '\n' then return [] 
    else do
        xs <- myGetLine1 
        return (x:xs)

--4
prelNo noin = sqrt noin
ioNumber = do
 noin <- readLn :: IO Float
 putStrLn $ "Intrare\n" ++ (show noin)
 let noout = prelNo noin
 putStrLn $ "Iesire"
 print noout

{-ioNumber' =
    (readLn :: IO Float) >>= \noin -> 
    putStrin ("Intrare\n" ++ (show noin)) >> 
    let noout = prelNo noin in 
    putStrin "Iesire" >> 
    print noout
    -}

data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN p =  "NAME: " ++ name p

showPersonA :: Person -> String
showPersonA p = "AGE: " ++  show (age p)

showPerson :: Person -> String
showPerson p = "(" ++ showPersonN p ++ ", " ++ showPersonA p ++ ")"

---------

newtype Reader env a = Reader { runReader :: env -> a }


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




        
      







    

