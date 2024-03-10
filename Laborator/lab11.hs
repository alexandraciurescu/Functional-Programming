data Pair a = Pair a a deriving Show

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

p= Pair 3 5

data Constant a b = Constant b
{-
instance Functor (Constant a) where
    fmap f (Constant b) = Constant f(b) -}

data Two a b = Two a b

instance Functor (Two a) where
    fmap f (Two x y ) = Two x (f y)

-----------------------------------------

data Three a b c = Three a b c
instance Functor  (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

data Three' a b = Three' a b b
instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)


data Four a b c d = Four a b c d

instance Functor (Four a b c) where
    fmap f (Four a b  c d) = Four a b c (f d)
       


data Four'' a b = Four'' a a a b

instance Functor (Four'' a) where
    fmap f (Four'' x y z c) = Four'' x y z (f c)


data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
    fmap _ Finance = Finance 
    fmap _ (Desk x)= Desk x
    fmap f (Bloor x) = Bloor (f x)

data LiftItOut f a = LiftItOut (f a)

lio = LiftItOut(Nothing) -- a: Int, f:Maybe a, f a : Maybe Int

instance Functor  f => Functor (LiftItOut f) where
    fmap g (LiftItOut fa) = LiftItOut(fmap g fa)


data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
    fmap :: (a -> b) -> GoatLord a -> GoatLord b
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat x) = OneGoat(f x)
    fmap f (MoreGoats gla glb glc)= MoreGoats (fmap f gla) (fmap f glb) (fmap f glc)

data TalkToMe a = Halt | Print String a | Read (String -> a)
 
instance Functor TalkToMe where
    fmap :: (a -> b) -> TalkToMe a -> TalkToMe b
    fmap _ Halt = Halt 
    fmap f (Print s x) = Print s (f x)
    fmap f (Read fa) = Read (f . fa)