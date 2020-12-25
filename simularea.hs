import Data.List
import Data.Char
--simulare test
import Data.Tuple


--1


ex1 :: Char -> Bool

ex1 a
    | a `elem` "abcdefghijklmABCDEFGHIJKLM" = True
    | isDigit a = False
    | otherwise = Prelude.error "eroare"


--1 b)
-- am corectat putin
g:: String -> Bool

g sir = ((length [ch | ch<- sir,isAlpha ch && ex1 ch]) `div` 2 ) < (length ([ch| ch<-sir, ch `elem` "abcdefghijklmABCDEFGHIJKLM"]))


--1 c)
-- de la profa

h :: String ->Bool

h "" = False

h list = length (h_aux list) > length (h_aux2 list)

h_aux [] = []
h_aux (x:t)
    | isAlpha x  && ex1 x = x :h_aux t
    | otherwise = h_aux t

h_aux2 [] = []
h_aux2 (x:t)
    | x `elem` "abcdefghijklmABCDEFGHIJKLM" = x:h_aux2 t
    | otherwise = h_aux t




--2a)

c:: [Int] -> [Int]

c sir = [sir!!snd a| a<-zip sir [0..length sir-2] , sir!!(snd a) ==sir!!((snd a) +1)]

--2b)



c2 :: [Int] -> [Int]

c2 sir
    |length sir==1 = []
    |head sir == head (tail sir) = [head sir] ++c2 (tail sir) 
    |otherwise = c2 (tail sir)

--2c)

prop_cd :: [Int]->([Int] ->[Int])-> ([Int]->[Int])->Bool
prop_cd sir f1 f2 = f1 sir == f2 sir






--simularea


f1 :: Char -> Bool

f1 ch
    |ch `elem` "abcdefghijklmABCDEFGHIJKLM" = True
    |(isAlpha ch == True) = False
    |otherwise = Prelude.error "eroare"

--1)B

g1 :: String -> Bool

g1 sir = g1aux sir > g1aux2 sir

g1aux :: String ->Int
g1aux sir = sum[1|a<-sir , isAlpha a && f1 a==True]

g1aux2 ::String ->Int
g1aux2 sir = sum[1|a<-sir ,isAlpha a && f1 a ==False]

--1c)

h1:: String -> Bool

h1 sir = g1auxrec sir >0

g1auxrec :: String ->Int
g1auxrec sir
    | null sir =0
    |  isAlpha a && f1 a==True = g1auxrec (tail sir) +1
    | isAlpha a && f1 a==False = g1auxrec (tail sir) -1
    |otherwise = g1auxrec( tail sir)
    where a=head sir


--2a)

c1  :: [Int] -> [Int]

c1 l = [l!!i| i<-[0..(length l -2)], l!!i == l!!(i+1) ]

--2b)

cRec :: [Int] -> [Int]

cRec l
    |length l==1 =[]
    |head l == head (tail l) = [head l] ++ cRec( tail l)
    |otherwise = cRec (tail l)

--2c)

prop_cd1 :: [Int] ->Bool

prop_cd1 l = c1 l == cRec l   

--revizuire
import Data.List
import Data.Char

--1
f ch 
    |ch `elem` "abcdefghijklmABCDEFGHIJKLM" = True
    | isAlpha ch = False
    |otherwise = error "eroare"

--1b)

g sir = length [a|a<- sir, a `elem` "abcdefghijklmABCDEFGHIJKLM"] > length [a|a<-sir, ((a `elem` "abcdefghijklmABCDEFGHIJKLM") ==False) && isAlpha a]

--1c)

gRecAux1 sir
    |null sir = 0
    | head sir `elem` "abcdefghijklmABCDEFGHIJKLM" = 1+ gRecAux1 (tail sir)
    | otherwise = gRecAux1 (tail sir)

gRecAux2 sir
    |null sir=0
    |isAlpha (head sir) && ((head sir `elem` "abcdefghijklmABCDEFGHIJKLM" )== False) = 1+gRecAux2 (tail sir)
    | otherwise = gRecAux2 (tail sir)

gRec sir = gRecAux1 sir > gRecAux2 sir

--2

ex2a l = [l!!i | i<-[0..length l-2],l!!i==l!!(i+1) ]

ex2b l
    |length l ==1 =[]
    |head l == head(tail l) = [head l] ++ ex2b (tail l)
    |otherwise = ex2b(tail l)

ex2c l = ex2a l == ex2b l