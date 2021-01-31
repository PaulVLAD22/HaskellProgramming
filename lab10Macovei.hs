data Expr 
    = Const Int 
    | Expr :+: Expr
    | Expr :*: Expr
    deriving Eq 
    
data Operation = Add | Mult
    deriving (Eq,Show)

data Tree
    =Lf Int 
    |Node Operation Tree Tree
    deriving(Eq,Show)

arb1= Node Mult (Node Add (Lf 2) (Lf 3))(Lf 5)

--ex 1
instance Show Expr where
    show (Const nr)=show nr
    show (p1 :+: p2)= show p1++"+"++show p2
    show (p1 :*: p2) = show p1++"*"++show p2

--ex2 
evalExp:: Expr-> Int
evalExp (Const nr )=nr
evalExp (p1 :+: p2) =evalExp p1+evalExp p2
evalExp (p1 :*: p2)=evalExp p1 * evalExp p2

exp1 = ((Const 2 :+: Const 3):+: (Const 0 :*: Const 5))

evalArb ::Tree ->Int
evalArb (Lf x) =x
evalArb (Node Add t1 t2) = evalArb t1 + evalArb t2
evalArb(Node Mult t1 t2)= evalArb t1 * evalArb t2

expToArb ::Expr -> Tree

expToArb (Const x)= Lf x
expToArb(p1 :+: p2) = Node Add (expToArb(p1)) (expToArb(p2))
expToArb(p1 :*: p2)= Node Mult (expToArb(p1)) (expToArb(p2))

class MySmallCheck a where
    smallValues :: [a]
    smallCheck :: (a->Bool) ->Bool
    smallCheck prop = and [prop x| x<-smallValues]


instance MySmallCheck Expr where
    smallValues =[exp1]

--sa se verifica ca evaluarea unei expresii este egala cu evaluarea arborelui asociat

checkExpr ::Expr->Bool 
checkExpr a = evalExp(a) == evalArb(expToArb(a))

--Class Collection

--definim clasa Collection

type Key = Int 
type Value = String 

class Collection c where
    cempty :: c 
    csingleton :: Key -> Value -> c
    cinsert :: Key -> Value -> c -> c
    cdelete :: Key -> c -> c
    clookup :: Key -> c -> Maybe Value
    ctoList:: c->[(Key,Value)]
    ckeys :: c-> [Key]
    ckeys c =[fst p| p<-ctoList(c)]

    cvalues :: c ->[Value]
    cvalues c=[snd p|p<-ctoList(c)]

    cFromList :: [(Key,Value)] -> c
    cFromList [] = cempty
    cFromList ((k,v):kvs)=cinsert k v (cFromList kvs)

newtype PairList = PairList {getPairList::[(Key,Value)]}

-- data PairList = PairList [(Key,Value)]
-- getPairList :: [(Key,Value)]
-- getPairList (PairList xs)=xs

--o instanta a clasei show

instance Show PairList where
    show (PairList pairlist) = "PairList"++ show pairlist

instance Collection PairList where
    cempty = PairList []
    csingleton k v = PairList [(k,v)]
    cinsert k v (PairList list) = if(clookup k (PairList list)==Nothing) then PairList ( list ++ [(k,v)]) else PairList list
    cdelete k (PairList list)= PairList [(k1,v1) | (k1,v1)<-list,k1/=k]
    clookup k (PairList list)=lookup k list
    ctoList (PairList list)= list

--aici implementam functiile care n au fost definite deja in clasa collection

data SearchTree
    =Empty
    | Nod SearchTree Key (Maybe Value) SearchTree
    deriving Show

--in st elemente cu cheie mai mica
-- in dr elemente cu cheie mai mare

instance Collection SearchTree where
    cempty = Empty
    csingleton k v = Nod Empty k (Just v)  Empty

--Functorii

data Arbore a
    =Frunza a
    |Nod2 a (Arbore a) (Arbore a)
    deriving Show
--instanta a functor pt tip arbore

-- primind functie (a->b) , un Arbore a => Arbore b

instance Functor Arbore where
    fmap f (Frunza a) = Frunza (f a)
    fmap f (Nod2 x left right) = Nod2 (f x) (fmap f left) (fmap f right)

arbore = Nod2 2 (Nod2 3 (Frunza 2) (Frunza 2)) (Frunza 5)

