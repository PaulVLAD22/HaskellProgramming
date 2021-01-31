myLookUp ::Int-> [(Int,String)]->Maybe String 

myLookUp _ [] = Nothing 
myLookUp element (x:xs)
    |element == fst x = Just(snd x)
    | otherwise  = myLookUp element xs

myLookUpFold element l = foldr (\(x,y) found -> if(x==element) then Just(y) else found) Nothing l

