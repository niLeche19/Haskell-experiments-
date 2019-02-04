doubleMe x = x + x  

doubleUs x y = doubleMe x + doubleMe y   

doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2   

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]  

longth xs = sum [1 | _ <- xs]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

myNum = 2
addTen n = n + 10
funcPlusTwo f n = f  ( n + 2 )
doubleIt n = n * 2
