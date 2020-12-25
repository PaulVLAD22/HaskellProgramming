import Test.QuickCheck
import Data.List
import Data.Char 
--lab7.pdf
myLookUp2 :: Int -> [(Int, String)] -> Maybe String
myLookUp2 _ [] = Nothing
myLookUp2 nr (x:xs)
    | nr == fst x && snd x == "" = Just ""
    | nr == fst x = Just (toUpper(head(snd x)): tail(snd x))
    | otherwise = myLookUp2 nr xs

testLookUp2 :: Int -> [(Int,String)] -> Bool
testLookUp2 x l = myLookUp2 x l == lookup x l

--DE DISCUTAT
--testLookUpCond2 :: Int -> [(Int,String)] -> Property
--testLookUpCond2 n list = foldr (&&) True (map (\x -> toUpper(head(snd x))==head(snd x)) list ) ==> testLookUp2 n list

capitalized :: String -> String
capitalized (h:t) = (toUpper h): t
capitalized [] = []

testLookUpCond2 :: Int -> [(Int,String)] -> Property
testLookUpCond2 n list = foldr (&&) True (map (\x -> (capitalized(snd x))== (snd x)) list ) ==> testLookUp2 n list

data ElemIS = I Int | S String
    deriving (Show,Eq)

myLookUpElem :: Int -> [(Int,ElemIS)]-> Maybe ElemIS
myLookUpElem _ [] = Nothing
myLookUpElem nr l
    | nr == fst (head l) = Just (snd (head l ))
    | otherwise = myLookUpElem nr (tail l)

testLookUpElem :: Int -> [(Int,ElemIS)]->Bool
testLookUpElem n list = myLookUpElem n list == lookup n list

--instance Arbitrary ElemIS where
--    arbitrary = oneof [geni,gens]
--        where
--        f=(unGen(arbitrary::Gen Int))
--        geni = MkGen (\s i->let x = f s i in (I x))
--        g=(unGen(arbitrary::Gen String))
--        gens = MkGen (\s i->let x = g s i in (S x))

--lab8 pdf

type Nume = String
data Prop
    = Var Nume   --Var p
    | F
    | T
    | Not Prop
    | Prop :|: Prop
    | Prop :&: Prop
    deriving (Eq,Read)

p1::Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p3::Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R")))




instance Show Prop where
    show (Var x) = x
    show F = "F"
    show T = "T"
    show (Not formula) = "(~" ++ show formula ++ ")"
    show ( formula1 :|: formula2 )= "(" ++ show formula1 ++ "|" ++show formula2 ++")"
    show (formula1 :&: formula2) = "(" ++ show formula1 ++ "&" ++show formula2 ++")"