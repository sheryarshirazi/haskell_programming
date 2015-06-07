--Question 1
--Syntax :type x
--where x a,b,c,d,e of the question

--Question 2
--with condition
safetail_0 :: [x] -> [x]
safetail_0 n = if (length n) == 0  then [] else (tail n)
-- or
safetail_01 :: [x] -> [x]
safetail_01 n = if null n then [] else tail n
--with Guarded equations

safetail_02 :: [x] -> [x]
safetail_02 n | null n = []
	| otherwise = tail n
--with pattern matching
safetail_03 :: [a]->[a]
safetail_03 [] = []
safetail_03 (x:xs) = xs

--Question 3
pyth :: (Int,Int,Int)->Bool
pyth (a, b, c) = if (a^2)+(b^2)==c^2 then True else False

--Question 4
factors n = [ x | x <- [1..n-1], n `mod` x==0]
perfect n = sum ( factors n ) == n