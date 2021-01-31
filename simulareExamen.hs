import Data.List
import qualified Data.Set as Set 

data Arbore a
    =Nod (Arbore a ) a (Arbore a)
    |Frunza a
    |Empty


inOrdine :: Arbore a-> [a]
inOrdine (Nod left mid right) =inOrdine(left) ++ [mid] ++ inOrdine(right)
inOrdine (Frunza a)= [a]
inOrdine (Empty)= []

isSorted :: (Ord a)=>Arbore a -> Bool 
isSorted (arb) = isSortedAux(inOrdine arb)

isSortedAux :: Ord a => [a] -> Bool
isSortedAux (x:xs)
    |length xs>=1 = x<= head xs && isSortedAux(xs)
    |otherwise =True 


arboreEx= Nod (Nod (Empty) 2 (Frunza 3)) 5 (Frunza 7)

noDuplicates:: (Ord a)=>Arbore a -> Bool 
noDuplicates (arb)= hasDuplicates (inOrdine arb)

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates list = length list /= length set
  where set = Set.fromList list


insertArbore (Nod left mid right) val
    |val<mid =Nod (insertArbore left val) mid right
    |val>mid =Nod left mid (insertArbore right val)
    | otherwise  = Nod left mid right
insertArbore (Frunza x) val
    |x<val = Nod (Frunza val) x (Empty)
    |x>val = Nod (Empty) x (Frunza val)
    |otherwise =Frunza x
insertArbore Empty val= Frunza val

instance Functor Arbore where
    fmap f (Nod left mid right) = Nod (fmap f left ) (f mid) (fmap f right)
    fmap f (Empty) = Empty
    fmap f (Frunza a )= Frunza (f a)

-- de la arbore
instance Foldable Arbore where
    foldMap _ Empty = mempty
    foldMap f (Frunza x) = f x
    foldMap f (Nod left x right) = foldMap f left `mappend` f x `mappend` foldMap f right

instance Show a => Show (Arbore a) where
    show (arb) = show (inOrdine (arb))
