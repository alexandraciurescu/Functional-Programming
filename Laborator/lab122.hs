{---}


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
    pure :: a -> List a
    pure x = Cons x Nil  -- Creăm o listă care conține doar elementul dat.
    Nil <*> _ = Nil  -- Dacă prima listă este Nil, rezultatul va fi mereu Nil.
    _ <*> Nil = Nil  -- Dacă a doua listă este Nil, rezultatul va fi mereu Nil.
    Cons f fs <*> xs = append (fmap f xs ) (fs <*> xs)  
-- Aplicăm funcția din prima listă asupra elementelor celei de-a doua liste și concatenăm rezultatele.