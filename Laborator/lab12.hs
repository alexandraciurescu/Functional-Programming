{--}


data List a = Nil
            | Cons a (List a)
        deriving (Eq, Show)

-- Funcția 'append' concatenează două liste.
append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x (append xs ys)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
instance Applicative List where
    pure x = Cons x Nil  -- Creăm o listă care conține doar elementul dat.
    Nil <*> _ = Nil  -- Dacă prima listă este Nil, rezultatul va fi mereu Nil.
    _ <*> Nil = Nil  -- Dacă a doua listă este Nil, rezultatul va fi mereu Nil.
    Cons f fs <*> xs = append (fmap f xs ) (fs <*> xs)  
-- Aplicăm funcția din prima listă asupra elementelor celei de-a doua liste și concatenăm rezultatele.



data Cow = Cow {
        name :: String
        , age :: Int
        , weight :: Int
        } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty x = Just x


noNegative :: Int -> Maybe Int
noNegative x
    |x<0 = Nothing
    |otherwise = Just x


test21 = noEmpty "abc" == Just "abc"
test22 = noNegative (-5) == Nothing 
test23 = noNegative 5 == Just 5 

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString = undefined 

test24 = cowFromString "Milka" 5 100 == Just (Cow {name = "Milka", age = 5, weight = 100})

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address
    deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength = undefined 

test31 = validateLength 5 "abc" == Just "abc"
mkName :: String -> Maybe Name
mkName = undefined 

mkAddress :: String -> Maybe Address
mkAddress = undefined 

test32 = mkName "Gigel" ==  Just (Name "Gigel")
test33 = mkAddress "Str Academiei" ==  Just (Address "Str Academiei")

mkPerson :: String -> String -> Maybe Person
mkPerson = undefined 

test34 = mkPerson "Gigel" "Str Academiei" == Just (Person (Name "Gigel") (Address "Str Academiei"))