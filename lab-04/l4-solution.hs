--Question 1
scalpro xs ys=sum [x*y |(x,y)<-zip xs ys]

--Question 2
pairs xs = zip xs (tail xs)
sorted xs=and[x<=y|(x,y)<-pairs xs]

--Question 3
factors n = [ x | x <- [1..n], n `mod` x==0]
is_prime n = factors n==[1,n]

--Question 4
primes n=[ x | x <-[2..n] , is_prime x]

--Question 5

in_list n xs = not (or [n==x | x <- xs])
missing xs = [x | x <- [1..range], in_list x xs] where range = head (reverse xs)

--Question 6 
findInput a = if a >= '0' && a<= '9' then "Number" else 
			  if a >='a' || a >='A' && a <='z' || a<='Z' 
			  then "Alphabets" else "sepcial characters"
--Question 7
asciis a = [x+48 | x <- [(a `div` 1000),(a `mod` 1000) `div` 100,
						 (a `mod` 100) `div` 10, a `mod` 10]]
