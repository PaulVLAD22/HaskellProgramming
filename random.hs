import Data.List
import Data.Char

 --colocviu anu 3

f :: String ->String
f sir
    |length sir ==1 =sir
    |head sir==head(tail sir) = f (tail sir)
    |otherwise = [head sir]++f(tail sir)

ex2 :: [Int]->Int

ex2 l = foldr (+) 0 (map (aux) (zip l (tail l)))

aux ::(Int,Int)->Int
aux (x,y)
    |x `rem` 3==0 && y`rem` 3 ==0 = x-y
    |otherwise = x*y

ex3 ::[Int] ->[Int]->Int

ex3 l1 l2
    |length l1==length l2 =foldr (+) 0 (map (aux3) (zip l1 l2))
    |otherwise =Prelude.error "lungimi diferite"

aux3 :: (Int,Int)->Int
aux3 (x,y)=x^2*y^2

ex4 :: String -> [String]

ex4 sir
    |null sir=[]
    |otherwise = [tail sir] ++ ex4 (tail sir)