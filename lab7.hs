import Data.Foldable
import Data.Functor
import Test.QuickCheck
import Data.List
data Linie = L [Int]
data Matrice = M [Linie]

verifica :: Matrice->Int->Bool


verifica (M m) n = foldr (&&) True [sum l == n | L l <- m]

--b
showLine :: Linie -> [Char]
showLine (L []) = ""
showLine (L (x:xs)) = show(x) ++ " " ++ showLine(L xs)

instance Show Matrice where
    show (M []) = ""
    show (M (x:[])) = (showLine x)
    show (M (x:xs)) = (showLine x) ++ "\n" ++ (show(M xs))

--c

doarPozN :: Matrice->Int->Bool

doarPozN (M m) n = foldr (&&) True [and(map (>0) l) | L l <- m , length l==n]

--pana acum a fost din lab6


--lab7 pdf


double :: Int -> Int
double x = x*2
triple x =x*3
penta x = x*5

test x = (double x +triple x) == (penta x)

myLookUp :: Int -> [(Int, String)] -> Maybe String
myLookUp _ [] = Nothing
myLookUp nr (x:xs)
| nr == fst x = Just (snd x)
| otherwise = myLookUp nr xs
