module PTfuncsyntax where
unshow x = (read :: String -> Int) x
{-
  Examples that make use of some additional syntax features within 
  functions, and use of recursion for writing functions.
  
  *** All functions must now include a type declaration! ***
  
  To help you get started, type declarations will be provided for you 
  in all templates. Any functions you define on your own should include
  a type declaration.
-}

{-
  Create a function called windChillRisk that accepts a temperature in 
  degrees F, and a wind speed in miles per hour and makes a statement
  about the danger of frostbite. 
  
  The formula for wind chill temperature is:
  Twc = 35.74 + 0.6215*Ta - 35.75*v**0.16 + 0.4275*Ta*v**0.16
  where Ta is the temperature (F) and v is the wind speed (mph).
  
  The relationship between wind chill temperature and frostbite danger is:
  Twc >= 16: "Low Risk"
  16 > Twc >= -17: "Moderate Risk"
  -17 > Twc >= -38: "High Risk"
  -38 > Twc >= -53: "Very High Risk"
  -53 > Twc >= -65: "Severe Risk"
  -64 > Twc : "Extreme Risk"
  
  Based on :
  https://www.weather.gov/safety/cold-wind-chill-chart
  http://www.ec.gc.ca/meteo-weather/default.asp?lang=n&n=5FBF816A-1#wc6
  
  Hint: See the BMI example in the Haskell tutorial.
  Hint: Eliminate redundant code!
  Hint: When you test your function with negative numbers, use parentheses.
  
  For example:
  *PTfuncsyntax> windChillRisk (-10) 25
  "High Risk"
  *PTfuncsyntax> windChillRisk 60 10
  "Low Risk"
  *PTfuncsyntax> windChillRisk (-15) 55
  "Severe Risk"
  *PTfuncsyntax> windChillRisk (-15) 50
  "Very High Risk"
-}

windChillRisk :: (Floating a, Ord a) => a -> a -> String
twc t v= 35.74 + 0.6215*t - 35.75*v**0.16 + 0.4275*t*v**0.16
windChillRisk t v 
    | risk >= 16 = "Low Risk."
    | risk >= -17 = "Moderate Risk"
    | risk >= -38 = "High Risk"
    | risk >= -53 = "Very High Risk"
    | risk >= -65 = "Severe Risk"
    | otherwise = "Extreme Risk"
    where risk = 35.74 + 0.6215*t - 35.75*v**0.16 + 0.4275*t*v**0.16


{-
  Create a function called yoCount that accepts a string and finds 
  the number of times "yo" or "Yo" appears in the string. Use RECURSION!
  
  For example:
  
  *PTfuncsyntax> yoCount "yo"
  1
  *PTfuncsyntax> yoCount "Yo mother is so sweet! yoyo, dog!"
  3  
-}

yoCount :: String -> Integer
-- replace the "-99" bits with your own code below
yoCount [] = 0 -- empty string passed in? How many yo is that?
yoCount (_:[]) = 0-- a single character passed in? How many yo is that?
yoCount ('y':'o':s) = 1 + (yoCount s)-- a string starting with "yo". What to do here?
yoCount ('Y':'o':s) = 1 + (yoCount s) -- a string starting with "Yo". What to do here?
yoCount s = yoCount (tail s)-- a string that doesn't start with "yo" or "Yo". What to do here?


{-
  Create a RECURSIVE function called interestTable that accepts a 
  principal amount, p, an interest rate r (as a percent), and the 
  number of periods (or years) to calculate the growth in principal.
  The function returns a list of numbers, starting with the original
  principal amount, that shows the principal value for each period 
  (rounded to whole dollars). Rounded values are NOT to be used in 
  calculating the next year's principal amount.
 
  Hint: Each year the new principal is computed from the previous
  year's principal according to: pnew = pold*(1 + 0.01*r)
  Hint: Haskell has a built-in round function.
 
  For example:
  
  *PTfuncsyntax> interestTable 100 10 5
  [100,110,121,133,146,161]
-}

-- interestTable :: (Eq a, Integral a1, Num a, RealFrac a2) => a2 -> a2 -> a -> [a1]
intCal p r n = [p * (1 + (0.01 * r))]
interestTable p r n 
                | n <= 0 = []
                | otherwise = p : intCal p r n 
                
table p r n = [round (p * ((1 + 0.01 * r) ^ x)) | x <- [0..n]] -- it's not recursive but it works...
                
-- What is the syntax for Haskell's round function?

-- a = p(0.01 * r)^n
{- 
    
  Create a function called charToPhoneDigit that accepts an upper and/or
  lower case character and converts it to a number according to the 
  number/letter mapping on a standard phone keypad. Only valid characters
  need to be handled
  Hint: Use pattern matching, guards or case? Which would be best?
  Hint: ABC=2, DEF=3, GHI=4, JKL=5, MNO=6, PQRS=7, TUV=8, WXYZ=9
  For example:
  
  *PTfuncsyntax> charToPhoneDigit 'p'
  7
  *PTfuncsyntax> charToPhoneDigit 'P' 
  7
-}

charToPhoneDigit :: Char -> Int
chars = zip [0..25] ['a'..'z'] ++ zip [0..25] ['A'..'Z']
nums' c = [fst x | x <- chars, snd x == c]
charToPhoneDigit c -- put your code here
                | nums <= 2   = 1
                | nums <= 5   = 2
                | nums <= 8   = 3
                | nums <= 11  = 4
                | nums <= 14  = 5
                | nums <= 17  = 6
                | nums <= 20  = 7
                | nums <= 23  = 8
                |otherwise = 9
                where nums = head [fst x | x <- chars, snd x == c]


{-
  Create a RECURSIVE function called numListToNum that accepts a list of
  positive or zero, single-digit integers and returns a number with those 
  digits,in the correct order.
  For example:
  
  *PTfuncsyntax> numListToNum [0,1,3,5,0]
  1350
-}


numListToNum :: [Int] -> Int
numListToNum' (x:[]) = show x
numListToNum' (x:xs) = show x ++ numListToNum' xs
numListToNum nums = (read :: String -> Int) (numListToNum' nums)
-- put your code here (more patterns needed?)


{- 
  Create a function called wordsToPhone that accepts a string of upper and/or
  lower case characters, possibly including spaces or punctuation characters, 
  and converts it to a number according to the number/letter mapping on a 
  standard phone keypad (ignoring the invalid characters).
  
  Hint: Use charToPhoneDigit and numListToNum. You may want to introduce 
        other small functions to help.
  For example:
   
  *PTfuncsyntax> wordsToPhone "pizza"
  74992
  *PTfuncsyntax> wordsToPhone "Good Morning, Vietnam"
  466366764648438626
-}

wordsToPhone :: String -> Int
chars' = ['a'..'z']++['A'..'Z']
wordsToPhone' (x : xs)
            | xs == [] = show (charToPhoneDigit x)
            | elem x chars' == False = wordsToPhone' xs
            | otherwise = (show (charToPhoneDigit x)) ++ (wordsToPhone' xs)

wordsToPhone w = (read :: String -> Int) (wordsToPhone' w)

