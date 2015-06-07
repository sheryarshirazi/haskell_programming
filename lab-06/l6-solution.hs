--q1
pairs xs= zip xs (tail xs)
isSorted xs= and [x<=y|(x,y)<-pairs xs]

--q2
factors n = [x |x<-[1..n], n `mod` x == 0]
prime n = factors n == [1,n]
lPrime n = [x |x<-[1..n],prime x]

--q3

logL xs = and xs

--q4
posx x xs = [i |(x',i) <-zip xs [0..n], x==x']
	where n = length xs-1

--q5
myGcd 0 d = d
myGcd n1 n2 = myGcd rem n1
	where
		rem=n2 `mod` n1

coPr n1 n2 = myGcd n1 n2 == 1

myPhi n = length [x |x<-[1..n],coPr x n]