-- Question 1
charNumber :: Char -> Bool
charNumber a = if a >= '0' && a<= '9' then True else False 

-- Question 2
asciis a = [x+48 | x <- [(a `div` 1000),(a `mod` 1000) `div` 100, (a `mod` 100) `div` 10, a `mod` 10]]

-- or

asciis_2 n = x1: x2:x3:x4:[]
	where n1 = n  `div` 1000
	      n2 = n  `mod` 1000
	      n3 = n2 `div` 100
	      n4 = n2 `mod` 100
	      n5 = n4 `div` 10
	      n6 = n4 `mod` 10
	      x1 = n1 +48
	      x2 = n3 +48
	      x3 = n5 +48
	      x4 = n6 +48

-- Question 3 find the day of the given date
which_day :: Int -> String
which_day date = if d==0 then "sat" else 
				 if d==1 then "sun" else 
				 if d==2 then "mon" else 
				 if d==3 then "tue" else 
				 if d==4 then "wed" else 
				 if d==5 then "thu" else "fri"
	where
		d=date `mod` 7

-- Question 4 
isEven n = if n `mod` 2 == 0 then True else False

-- Question 5
isOdd n = if n `mod` 2 == 1 then True else False

-- Question 6
isList n = ([x | x <- [1..n],isOdd x == True],[x | x <- [1..n],isEven x == True])

-- Question 7
positions :: Eq a => a -> [a] -> [Int]
positions x xs =
   [i | (x',i) <- zip xs [0..n], x == x']
   where n = length xs - 1

-- Question 8
isThere :: Eq a => a -> [a] -> Bool
isThere x ys = if positions x ys == [] then False else True



