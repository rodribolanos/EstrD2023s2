hayAlMenosUnCinco :: [Int] -> Bool
hayAlMenosUnCinco [] = False
hayAlMenosUnCinco (x:xs) = x == 5 || hayAlMenosUnCinco xs

esCinco :: Int -> Bool 
esCinco 5 = True 
esCinco _ = False 

hayAlMenosUn :: Int -> [Int] -> Bool 
hayAlMenosUn _ [] = False
hayAlMenosUn a (x:xs) = x == a || hayAlMenosUn a xs 

esElNumero :: Int -> Bool 
esElNumero a = a


soloLosMayoresQue :: Int -> [Int] -> [Int]
soloLosMayoresQue n [] =
soloLosMayoresQue n (x:xs)       

