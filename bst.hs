import Data.Monoid
import Data.List

data Arbore a = Nod (Arbore a) a (Arbore a) | Frunza a | Empty

exampleTree = Nod (Nod (Frunza 1) 2 (Frunza 3)) 5 (Nod (Frunza 6) 9 (Frunza 10))

instance Functor Arbore where
    fmap f (Frunza x) = Frunza (f x)
    fmap _ Empty = Empty
    fmap f (Nod left x right) = Nod (fmap f left) (f x) (fmap f right)

instance Foldable Arbore where
    foldMap _ Empty = mempty
    foldMap f (Frunza x) = f x
    foldMap f (Nod left x right) = foldMap f left `mappend` f x `mappend` foldMap f right

instance Show a => Show (Arbore a) where
    show tree = intercalate ", " $ map show $ inOrder tree

inOrder :: Arbore a -> [a]
inOrder Empty = []
inOrder (Frunza x) = [x]
inOrder (Nod left x right) = inOrder left ++ [x] ++ inOrder right

--[1,2,3,4] ++ [5] ++ [6,9,10]

isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:xs) = x < (head xs) && isSorted xs

isBST :: (Ord a) => Arbore a -> Bool
isBST a = isSorted(inOrder a)


insertBST :: (Eq a, Ord a) => Arbore a -> a -> Arbore a
insertBST Empty val =  Frunza val
insertBST (Frunza x) val
    | x == val = Frunza x
    | x < val = Nod Empty x (Frunza val)
    | x > val = Nod (Frunza val) x Empty
insertBST (Nod left x right) val
    | x == val = Nod left x right
    | val < x = Nod (insertBST left val) x right
    | val > x = Nod left x (insertBST right val)