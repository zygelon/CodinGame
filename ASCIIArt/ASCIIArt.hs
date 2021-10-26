import System.IO
import Control.Monad
import Data.Char

data AlphaData = AlphaData {lineLength :: Int
                           , letters :: [String]}

convertSymbToNum symbol =
    if isAlpha symbol
    then
        let upSymbol = toUpper symbol
        in ord upSymbol -  ord 'A'
    else 1 + ord 'Z' - ord 'A'

getFullLetter :: AlphaData -> Int -> [String]
getFullLetter letterNum alphaData =
    let alphaDataNextIter :: Int -> AlphaData -> [String]
        alphaDataNextIter _ (AlphaData _ []) = []
        alphaDataNextIter letterNum (AlphaData l (x:xs)) =
           let chopedLeft = drop (letterNum * l) x
           in  take l chopedLeft : alphaDataNextIter letterNum (AlphaData l xs)
    in alphaDataNextIter alphaData letterNum

letterToStr :: [String] -> String
letterToStr [] = []
letterToStr (x:xs) =
    x ++ ['\n'] ++ letterToStr xs

convertWord alphaData [] = []
convertWord alphaData (x:xs) =
    getFullLetter alphaData (convertSymbToNum x) +++ (convertWord alphaData xs)

(+++) a b | null a = b
(+++) a b | null b = a
(+++) (x1:xs1) (x2:xs2) = (x1 ++ x2) : (xs1 +++ xs2)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let l = read input_line :: Int
    input_line <- getLine
    let h = read input_line :: Int
    wordToConvert <- getLine

    alphabet <- replicateM h getLine
    putStrLn $ letterToStr $ convertWord (AlphaData l alphabet) wordToConvert
    -- hPutStrLn stderr "Debug messages..."
    return ()