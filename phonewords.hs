{-
    This program will build on dictionary.hs and wordsToPhone from a previous
    assignment. You can copy your wordsToPhone source code here or you can simply
    include the line:
    
    import PTfuncsyntax
    
    and run this program in the same directory with your PFfuncsyntax.hs file.
    
    This program will ask the user to enter a 4-digit number. It will then list 
    off all of the english words that can be formed from that number on a standard 
    telephone keypad.
    
    Example of use:
    
    *Main> main
    Type a four-digit number:
    2376
    "Afro"
    "Bern"
    "berm"
    *Main> 

-}
import PTfuncsyntax
toInt = (read :: String -> Int)
main = do
    lst <- readFile "/usr/share/dict/american-english"
    let word = words lst
    putStrLn "Type a four-digit number: "
    str <- getLine
    let lstfinal = [show x | x <- word , wordsToPhone x == toInt str]
    mapM_ putStrLn lstfinal