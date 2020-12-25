import Test.QuickCheck;
import Data.Char
--lab4

produsRec :: [Integer] -> Integer
produsRec l
 | null t = h
 | otherwise = h * produsRec t
    where 
        h = head l
        t = tail l


produsFold :: [Integer] -> Integer
produsFold l = foldr (*) 1 l

prop_produs x = produsRec x == produsFold x

andRec :: [Bool] -> Bool
andRec l
 | null l = True
 | h = andRec t
 | otherwise = False
    where 
        h = head l
        t = tail l


andFold :: [Bool] -> Bool
andFold l = foldr (&&) True l

concatRec :: [[a]] -> [a]
concatRec l
 | null l = []
 | otherwise = h ++ concatRec t
    where 
        h = head l
        t = tail l


concatFold :: [[a]] -> [a]
concatFold l = foldr(++) [] l

rmChar :: Char -> String -> String
rmChar c sir = [x|x<-sir, x/= c]



--revizuire

produsRec1 :: [Int] -> Int

produsRec1 l
  |null l =1
  |otherwise = produsRec1 (tail l) * head l

produsFold1 :: [Int] -> Int

produsFold1 l = foldr (*) 1 l

andFold1:: [Bool] -> Bool

andFold1 l = foldr (&&) True l


concatRec1 l
  |null l = []
  |otherwise = head l ++ concatRec1 (tail l)

concatFold1 l = foldr (++) [] l


--4

rmChar1 :: Char->String -> String

rmChar1 ch sir
  |null sir = ""
  |head sir ==ch = rmChar1 ch (tail sir)
  |otherwise = [head sir] ++ rmChar1 ch (tail sir)

rmCharsRec :: String -> String -> String

rmCharsRec sir sir2
  |null sir =""
  |head sir `elem` sir2 = rmCharsRec (tail sir) sir2
  |otherwise = [head sir] ++ rmCharsRec (tail sir) sir2




--revizuire

produsFold2 l = foldr (*) 1 l

andRec1 l
  | null l =True
  | head l = True && andRec1 (tail l)
  |otherwise = False

andFold2 l = foldr (&&) True l

concatRec2 l
  |null l=[]
  |otherwise = head l ++ concatRec2 (tail l)

concatFold2 l = foldr (++) [] l

rmChar2 ch l =[a| a<-l , a/=ch]

rmCharsRec2 sir1 sir2 = [a| a<-sir2 , (a `elem` sir1)==False ]

rmCharsFold1 sir1 sir2 = foldr (:) [] (filter (`aux` sir1) sir2)

aux ch sir1  = (ch `elem` sir1 )==False

--revizuire

produsRec2 l
  |null l =1
  |otherwise = head l * (produsRec2  (tail l))

produsFold3 l = foldr (*) 1 l

andRec2 l
  |null l =True
  |otherwise = head l && (andRec2(tail l))

