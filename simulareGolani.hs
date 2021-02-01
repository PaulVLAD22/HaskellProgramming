import Data.List

data Mancare = Cozonac Int | Sarmale Int deriving (Show, Eq)

data Masa = Farfurii [Mancare] deriving Show

-- 1
isValid :: Masa -> Bool
isValid (Farfurii m) = sum [ x | (Cozonac x) <- m ] >= 2 && sum [ x | (Sarmale x) <- m ] >= 3 

isValid' :: Masa -> Bool
isValid' (Farfurii x) = isValid' x 0 0
    where
        isValid' [] s c
            | s >= 3 && c >= 2 = True
            | otherwise = False 
        isValid' (x:xs) s c = isValid' xs (s + isSarma x) (c + isCozonac x)
        isSarma (Sarmale x) = x
        isSarma _ = 0
        isCozonac (Cozonac x) = x
        isCozonac _ = 0

testValid :: Bool
testValid = isValid (Farfurii [Cozonac 3, Sarmale 2]) == False && isValid (Farfurii [Cozonac 3, Sarmale 3]) == True

prim :: Int -> Bool 
prim n = prim' n 2
    where
        prim' n d
            | d >= n `div` 2 = True
            | n `mod` d == 0 = False
            | otherwise = prim' n (d + 1)

meseValide :: [Masa] -> [Masa]
meseValide xs = [ Farfurii l | (Farfurii l) <- xs, prim (length l) && isValid (Farfurii l) ]

v = meseValide [Farfurii [Cozonac 3, Sarmale 5], Farfurii [Cozonac 1, Sarmale 1, Cozonac 3]]

instance Eq Masa where
    Farfurii x == Farfurii y = sort x == sort y

l = Farfurii [Cozonac 3, Cozonac 2, Sarmale 5] == Farfurii [Sarmale 5, Cozonac 5]

instance Ord Mancare where
    Cozonac x <= Cozonac y = x <= y
    Sarmale x <= Sarmale y = x <= y
    Sarmale x <= Cozonac y = x <= y
    Cozonac x <= Sarmale y = x <= y

mancareToInt :: Mancare -> Int
mancareToInt (Sarmale x) = 2 * x
mancareToInt (Cozonac x) = x

instance Ord Masa where
    Farfurii x <= Farfurii y = sum (map mancareToInt x) <= sum (map mancareToInt y) || (sum (map mancareToInt x) == sum (map mancareToInt y)) && length x <= length y
