-- lab 01
--quadratic 
quad :: (Float, Float, Float) -> (Float, Float)
quad (a, b, c) = (x1,x2)
		where
			x1	= (-b + sqrt y )/ (2*a)
			x2	= (-b - sqrt y )/ (2*a)
			y	= b * b - 4 * a * c

-- last element of the list
last_elem xs = head (reverse xs)
