--lab 9

data Expr = Const Int
    | Expr :+: Expr
    | Expr :*: Expr
      deriving Eq

data Operation = Add | Mult deriving (Eq,Show)

data Tree = Lf Int
    | Node Operation Tree Tree
    deriving (Eq,Show)


instance Show Expr where
    show (Const x) = show x
    show (a :+: b) = "(" ++ (show a) ++ " + " ++ show b ++ ")"
    show (a :*: b) = "(" ++ (show a) ++ " * " ++ show b ++ ")"

evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (a :+: b) = (evalExp a) + evalExp b
evalExp (a :*: b) = (evalExp a) * evalExp b

evalArb :: Tree->Int
evalArb (Lf x) = x
evalArb (Node Add a b)=evalArb a + evalArb b
evalArb (Node Mult a b)=evalArb a * evalArb b

expToArb :: Expr -> Tree
expToArb (Const x) = Lf x
expToArb (a :+: b) = Node Add (expToArb a) (expToArb b)
expToArb (a :*: b) = Node Mult (expToArb a) (expToArb b)

class MySmallCheck a where
smallValues :: [a]
smallCheck :: ( a -> Bool ) -> Bool
smallCheck prop = and [ prop x | x <- smallValues ]

instance MySmallCheck Expr where
smallValues = [exp1, exp2, exp3, exp4]

propEquals :: Expr -> Bool
propEquals e1 = (evalExp e1) == evalArb (expToArb e1)