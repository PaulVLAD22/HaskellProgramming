import Test.QuickCheck;

sumeImpare :: [Integer] ->Integer
sumeImpare [] = 0
sumeImpare (a:as)
    | odd a  = a*a + sumeImpare as
    | otherwise = sumeImpare as
--suma patrate

--cu fold
sumaImpareFold :: [Integer] -> Integer
sumaImpareFold = foldr op unit
    where
        unit=0
        a `op` suma
            |odd a = a*a+suma
            | otherwise = suma


-- cand dam where a `op` suma ii dam o valoare

-- foldrr face in mod normal a1 `op` a2 `op` a3...
-- si noi schimbam ce returneaza de ex cand face a1 `op` a2

-- scrie exemplul cu map si map_fold

filterFold :: (a->Bool) -> [a] ->[a]

filterFold p = foldr op unit
    where  
        unit = []
        a `op` filtered
            | p a = a :filtered
            | otherwise = filtered

--(mai devreme am scris functii cu foldr)



--ex1

semn :: [Integer] ->String

semn l
    |null l =""
    |head l>0 && head l<10 = "+" ++ semn( tail l)
    |head l<0 && head l>(-10) = "-" ++ semn(tail l)
    |head l ==0 = "0" ++ semn (tail l)
    |otherwise = semn (tail l)


semnClasa :: [Integer] -> String
semnClasa [] =[]
semnClasa (h:t) 
    | elem h [1..9] ='+' :rest
    | h==0 ='0' :rest
    |elem h [-9.. -1] = '-' :rest
    |otherwise = rest
    where rest = semnClasa t
    
semnFold :: [Integer] -> String
semnFold = foldr op unit
   where
     unit = []
     a `op` rest
        | a < 10 && a > 0     = '+' : rest
        | a > -10 && a < 0    = '-' : rest
        | a == 0              = '0' : rest
        | otherwise           = rest

--intelege mai bine tranzitia asta de la recursivitate la foldr


--MATRICI



corect :: [[a]] -> Bool
corect m
 | null t = True
 | length h1 == length h2 = corect t
 | otherwise = False
    where
        h1 = head m
        t = tail m
        h2 = head t


corect2 :: [[a]]->Bool
corect2 matrice = foldr(\(x,y) b -> x==y && b) True (zip list (tail list))
    where
        list = map (\x -> length x) matrice 

--ex2 

el :: [[a]] -> Int -> Int -> a
el m x y = (m !! x) !! y

enumera ::[a] -> [(a,Int)]
enumera list = zip list [0..]

insereazaPozitie :: ([a,Int],Int) -> [(a,Int,Int)]
insereazaPozitie (lista,lista) =
    map (\(x,coloana)->(x,linie,coloana)) lista

transforma :: [[a]] -> [(a,Int,Int)]
transforma matrix = concat (map insereazaPozitie (enumera (map enumera matrix)))

--nu stiu ce se intampla de la enumera in jos





--CHIAR TRE SA TE UITI IAR PE FOLDR