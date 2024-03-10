import Text.ParserCombinators.Parsec (label)
data Fruct
 = Mar String Bool
 | Portocala String Int


ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala "Tarocco" _ ) = True
ePortocalaDeSicilia (Portocala "Moro" _) = True
ePortocalaDeSicilia (Portocala "Sanguinello" _) = True
ePortocalaDeSicilia _ = False

isMarViermi :: Fruct -> Bool
isMarViermi (Mar _ True) = True
isMarViermi _ = False

nrMereViermi :: [Fruct] -> Int
nrMereViermi ls = length (filter isMarViermi ls)


type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

vorbeste :: Animal ->String 
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) ="Woof!"

rasa :: Animal -> Maybe String
rasa (Caine _ r) = Just r
rasa (Pisica _) = Nothing

data Linie = L [Int]
    deriving Show
data Matrice = M [Linie]
    deriving Show

sumaLista :: [Int] -> Int
sumaLista ls = foldr (+) 0 ls

verifica :: Matrice -> Int -> Bool
verifica (M linii) n = foldr (\(L l) acc -> acc && (sumaLista l == n)) True linii

doarpozitive :: [Int] -> Bool
doarpozitive l = foldr (\ x acc -> acc && x>0) True l

doarPozN :: Matrice -> Int ->Bool
doarPozN (M linii) n = 
    foldr (\(L l) acc -> acc && (doarpozitive l == True)) True [(L l)| (L l)<-linii, length l == n]


corect :: Matrice -> Bool
corect (M []) = True
corect (M (L l:ls)) = foldr (\(L l') acc -> length l ==length l' && acc) True ls