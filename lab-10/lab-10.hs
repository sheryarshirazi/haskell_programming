--q1
insertAt position element xs = take (position-1) xs ++ [element] ++ drop  position xs

--q2
toBin 0 = [0]
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
        | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]
--q3
units = ["","one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve",
		 "thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
tens = ["","","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

numWords n = 
	if n > 99 then
		if (n`rem`100) < 20 then
			units !! (n `div` 100) ++ " hundred " ++ units !! (n`rem` 100)
		else
			units !! (n `div` 100) ++ " hundred " ++ tens !! ((n`rem`100)`div`10) ++ " " ++ units !! (n `rem` 10)
	else
	 	if n < 20 then
	 		units !! n
	 	else
	 		tens !! (n `div` 10) ++ " " ++ units !! (n `rem` 10)
