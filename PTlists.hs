module PTlists where

{-
  Examples that make use of lists, ranges, list comprehensions. Some Haskell
  functions that might be useful: head, tail, init, last, length, null, reverse,
  take, drop, minimum, maximum, sum, product, elem, cycle, repeat, fst, snd.
-}

{- 
  Create a function called allLowerCaseLetters that returns a list of characters
  from 'a' to 'z'. Do not use the string: "abcdefghijklmnopqrstuvwxyz" in your
  function!
-}

allLowerCaseLetters = ['a'..'z'] -- put your code here

{- 
  Create a function called allIntegersInclusive that accepts two integer values,
  a and b, and returns a list of all integers from a to b, inclusive. If b<a then
  the list must be in descending order. For example:
  
  Prelude> allIntegersInclusive 3 7
  [3,4,5,6,7]
  Prelude> allIntegersInclusive 7 3
  [7,6,5,4,3]
-}

allIntegersInclusive a b = if a < b-- put your code here
                            then [a..b]
                            else reverse [b..a]

{-
  Create a function called allIntegersExcludingFirst that accepts two integer
  values, a and b, and returns a list of all integers from a to b, exclusive of 
  a and inclusive of b. If b<a then the list must be in descending order.
  For example:
  
  Prelude> allIntegersExcludingFirst 6 2
  [5,4,3,2]
-}

allIntegersExcludingFirst a b = if a < b
                                then tail [a..b]
                                else tail (reverse [b..a])
{- 
  Create a function called allThousands that returns a list of integers beginning
  with 1000 and ending with 9999.
-}

allThousands = [1000..9999]-- put your code here

{-
  Create a function called integersUpDownUp that accepts four integer values,
  a, b, c, d and returns a list of all integers between a and b (inclusive), 
  followed by all integers between b and c (excluding b), followed by all integers
  between c and d (excluding c). For example:
  
  Prelude> integersUpDownUp 3 6 3 10
  [3,4,5,6,5,4,3,4,5,6,7,8,9,10]
  Prelude> integersUpDownUp 6 2 3 10
  [6,5,4,3,2,3,4,5,6,7,8,9,10]
-}

integersUpDownUp a b c d = (if a < b 
then [a..b] 
else reverse [b..a]) ++ (if b < c 
then tail [b..c] 
else tail (reverse [c..b])) ++ (if c < d 
then tail [c..d]
else tail (reverse [d..c]))


{-
  Create a function called isDivisor that accepts two integers n and d and
  returns True if d is a divisor of n (hint: use the mod function). 
  For example:
  
  Prelude> isDivisor 6 2
  True
  Prelude> isDivisor 6 4
  False
-}

isDivisor n d = if n `mod` d == 0-- put your code here
then True
else False

{- 
  Create a function called divisors that accepts an integer, n, and returns
  a list of all its divisors (excluding n itself). For example:
  
  Prelude> divisors 12
  [1,2,3,4,6]
  
  Hint: use a list comprehension.
-}

divisors n = [x | x <- [1..n], n `mod` x == 0, x /= n]-- put your code here

{-
  Create a function called isPerfect that accepts an integer, n, and returns 
  True/False depending on whether the number is "perfect". A perfect number 
  is one in which the sum of its divisors equals the number itself. 
  For example, the number 6 (sum of divisors, 1, 2, 3 is 6):
  
  Prelude> isPerfect 6
  True
-}

isPerfect n = if sum (divisors n) == n -- put your code here
then True
else False

{-
  Create a function called allPerfects that accepts an integer, n, and returns
  a list of all perfect numbers between 1 and n (inclusive). For example:
  
  Prelude> allPerfects 30
  [6,28]
-}

allPerfects n = [x | x <- [1..n], isPerfect x == True]-- put your code here

{-
  Create a function called isRigid that accepts an integer, n, and returns
  True/False depending on whether the number is "rigid" (an invented name).
  A "rigid" number is one for which the sum of its digits equals the 
  product of its digits. For example, the number 22 is rigid because 2+2 == 2*2.
  For example: 
  
  Prelude> isRigid 123
  True
  
  Hint: The show function will convert a number to string.
  Hint: (read :: String -> Int) will convert an integer in string form to
  an integer. For example:
  
  Prelude> (read :: String -> Int) "3"
  3
  
  Hint: In Haskell, 'a' is not the same as "a".
  Hint: Break the problem down into sub-functions that handle individual pieces
  of the problem.
-}                                  -- put your code here
mString s =  [x : [] |x  <- (show s)]
mStr s = [(read :: String -> Int) x | x <- mString s]
isRigid n = if sum (mStr n) == product (mStr n)
then True 
else False

{-
  Create a function called allRigids that accepts an integer, n, and returns
  a list of all rigid numbers between 1 and n (inclusive). For example:
  
  Prelude> allRigids 1000
  [1,2,3,4,5,6,7,8,9,22,123,132,213,231,312,321]
-}

allRigids n = [x | x <- [0..n], isRigid x == True]-- put your code here

{-
  Create a function called fizzBuzz that accepts integers n, f, and b. 
  Return a list of numbers from 1 to n, in string form, with the 
  exception that if the number is divisible by f then the string is 
  "Fizz", if divisible by b it is "Buzz", and if it is divisible by 
  both f and b then it is "FizzBuzz". For example
  
  Prelude> fizzBuzz 10 2 3
  ["1","Fizz","Buzz","4","5","FizzBuzz","7","Fizz","Buzz","10"]
-}
check n f b = if n `mod` f == 0 && n `mod` b == 0 
then "FizzBuzz"
else if n `mod` b == 0
then "Buzz"
else if n `mod` f == 0 
then "Fizz"
else (show n)

fizzBuzz n f b = [check x f b | x <- [1..n]]-- put your code here

