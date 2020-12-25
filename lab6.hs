---lab 6

--a)
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia Mar _ _ = False
ePortocalaDeSicilia Portocala s i = s == "Moro" || s == "Tarocco" || s == "Sanguinello"

--b)
nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia l
 | null l = 0
 | ePortocalaDeSicilia h = nr + (nrFeliiSicilia t)
 | otherwise = nrFeliiSicilia t
    where
        h = head l
        t = tail l
        nr = nrFelii h
       
nrFelii (Portocala x y) = y


--c)
nrMereViermi :: [Fruct] -> Int
nrMereViermi xs = length[x | Mar _ x <- xs, x == True]

