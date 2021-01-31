
data Fruct =
    Mar String Bool
    |Portocala String Int
    deriving (Eq,Show)

-- Mar si Portocala sunt constructori de data

ePortocalaDeSicilia :: Fruct ->Bool

ePortocalaDeSicilia (Portocala soi _) = soi=="Torocco" || soi=="Moro"|| soi=="san"
ePortocalaDeSicilia (Mar _ _) = False

nrFeliiSicilia :: [Fruct] -> Int

nrFeliiSicilia []= 0
nrFeliiSicilia ((Portocala soi nr):fs)
    | ePortocalaDeSicilia(Portocala soi nr)=nr+nrFeliiSicilia(fs)
    | otherwise = nrFeliiSicilia fs
nrFeliiSicilia((Mar _ _) :fs) = nrFeliiSicilia fs

nrMereViermi :: [Fruct]->Int

nrMereViermi []=0
nrMereViermi ((Mar soi cuViermi):fs)
    | cuViermi==True = 1+nrMereViermi(fs)
    |otherwise = nrMereViermi(fs)
nrMereViermi((Portocala _ _):fs) = nrMereViermi(fs)

data Linie = L[Int]
    deriving Show
    
data Matrice = M [Linie]

matrice = M[L[1,2,3,4],L[5,5],L[1,3,6]]

verifica :: Matrice->Int->Bool

verifica (M []) _ = True

verifica (M (L x:xs)) nr
    | foldr (+) 0 x ==nr  =verifica(M xs) nr
    | otherwise =False

