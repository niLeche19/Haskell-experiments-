module PTfunctions where

quadraticVertex a b c = -b/2*a

quadraticDiscriminant a b c = b^2 - 4*(a*c)

quadraticRealSolutionQty a b c = if quadraticDiscriminant a b c == 0
                                    then 1
                                    else if quadraticDiscriminant a b c < 0
                                    then 0
                                    else 2

convertFtoC f = (f-32)/1.8

convertCtoF c = (1.8c)+32

convertTemp t s = if s == "f"
                    then (t-32) / 1.8
                    else if s == "c"
                    then 1.8*t + 32
                    else 100
                    