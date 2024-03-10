data Expr = Const Int -- integer constant
    | Expr :+: Expr -- addition
    | Expr :*: Expr -- multiplication
      deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
            | Node Operation Tree Tree -- branch
              deriving (Eq, Show)


instance Show Expr where
 show (Const x) = show x
 show (e1 :+: e2) = "(" ++ show e1 ++ " + "++ show e2 ++ ")"
 show (e1 :*: e2) = "(" ++ show e1 ++ " * "++ show e2 ++ ")"

--1
evalExp :: Expr -> Int
evalExp (Const n) = n
evalExp (e1 :+: e2) = evalExp e1 + evalExp e2
evalExp (e1 :*: e2) = evalExp e1 * evalExp e2

--2
evalArb :: Tree -> Int
evalArb (Lf n) = n
evalArb (Node Add e1 e2) = evalArb e1 + evalArb e2
evalArb(Node Mult e1 e2) = evalArb e1 * evalArb e2

--3
expToArb :: Expr -> Tree
expToArb (Const n) = Lf n
expToArb (e1 :+: e2) = Node Add (expToArb e1) (expToArb e2)
expToArb (e1 :*: e2) = Node Mult (expToArb e1) (expToArb e2)

--4
data IntSearchTree value
   = Empty
    | BNode
        (IntSearchTree value) -- elemente cu cheia mai mica
        Int -- cheia elementului
        (Maybe value) -- valoarea elementului
        (IntSearchTree value) -- elemente cu cheia mai mare


lookup' :: Int -> IntSearchTree value -> Maybe value
lookup' _ Empty = Nothing
lookup' key (BNode leftTree currentKey currentValue rightTree)
  | key == currentKey = currentValue
  | key < currentKey  = lookup' key leftTree
  | otherwise         = lookup' key rightTree

--5
keys :: IntSearchTree value -> [Int]
keys Empty = []
keys (BNode leftTree currentKey currentValue rightTree) =  keys leftTree ++ [currentKey] ++ keys rightTree

--6
values :: IntSearchTree value -> [value]
values Empty = []
values (BNode leftTree currentKey currentValue rightTree) =  values leftTree ++ maybeToList currentValue ++ values rightTree
   where
    maybeToList (Just x) = [x]
    maybeToList Nothing  = []

--7
insert :: Int -> value -> IntSearchTree value -> IntSearchTree value
insert key val Empty = BNode Empty key (Just val) Empty
insert key val (BNode leftTree currentKey currentValue rightTree)
 | key == currentKey = BNode leftTree key (Just val) rightTree
 | key<currentKey = BNode (insert key val leftTree) currentKey currentValue rightTree
 | otherwise = BNode leftTree currentKey currentValue (insert key val rightTree)

--8
delete :: Int -> IntSearchTree value -> IntSearchTree value
delete key Empty = Empty
delete key (BNode leftTree currentKey currentValue rightTree) 
   | key==currentKey = BNode leftTree currentKey Nothing rightTree
   | key<currentKey = BNode (delete key leftTree) currentKey currentValue rightTree
   | key>currentKey = BNode leftTree currentKey currentValue (delete key rightTree)

--9
toList :: IntSearchTree value -> [(Int, value)]
toList Empty = []
toList (BNode leftTree currentKey currentValue rightTree) =
  toList leftTree ++ maybeToList (currentKey, currentValue) ++ toList rightTree
  where
    maybeToList (key, Just value) = [(key, value)]
    maybeToList (_, Nothing)      = []

--10
insertPair :: (Int, value) -> IntSearchTree value -> IntSearchTree value
insertPair (key, val) Empty = BNode Empty key (Just val) Empty
insertPair (key, val) (BNode leftTree currentKey currentValue rightTree)
  | key < currentKey = BNode (insertPair (key,val) leftTree) currentKey currentValue rightTree
  |key >currentKey = BNode leftTree currentKey currentValue (insertPair (key,val) rightTree)
  | otherwise = BNode leftTree currentKey (Just val) rightTree

fromList :: [(Int, value)] -> IntSearchTree value
fromList = foldr insertPair Empty


--11
printTree :: IntSearchTree value -> String
printTree Empty = ""  -- Cazul de bază: arborele vid
printTree (BNode left key _ right) =
    "(" ++ printTree left ++ " " ++ show key ++ " " ++ printTree right ++ ")"  -- Concatenăm reprezentările liniare ale subarborilor și cheia nodului
