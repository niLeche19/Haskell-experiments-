module PTfunctions where

quadraticVertex a b = -b/2*a

quadraticDiscriminant b a = b^2 - 4*a


convertFtoC f = (f-32)/1.8

convertCtoF c = (1.8c)+32

convertTemp t s = if s == "f"
                    then (t-32) / 1.8
                    else if s == "c"
                    then 1.8*t + 32
                    else 100
                    