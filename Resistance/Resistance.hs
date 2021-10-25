import System.IO
import Control.Monad
import Data.List (elemIndex)

readInput :: Int -> Int -> IO [String]
readInput currResist numResists = do
    if currResist >= numResists
        then return []
        else do
            line <- getLine
            recursiveResult <- readInput (currResist + 1) numResists
            return (line : recursiveResult)

parseResistorsData :: [String] -> [(String, Float)]
parseResistorsData [] = []
parseResistorsData (x:xs) =
    let resistorName = head $ words x
        resistorPower = read (last $ words x) :: Float
    in (resistorName, resistorPower) : parseResistorsData xs

data CalcType = EParallel | ESeries | Nothing deriving(Enum)

getSubstrFor bracket list =
    let Just foundIndex = elemIndex bracket list
    in take foundIndex list

getResistance 

calcResistMain :: CalcType -> [(String, Float)] -> [String] -> Float
calcResistMain _ _ [] = 0.0
calcResistMain _ _ list | head list == "]" = 0.0
calcResistMain _ _ list | head list == ")" = 0.0
calcResistMain calcType _ (x:xs) | x == "[" =
    let sublist = getSubstrFor "]" xs
        innerParallelScope = calcResistMain EParallel resistsData sublist
        nextRecursResult = calcResistMain calcType resistsData (drop (length sublist) xs )
    in 1.0 / (innerParallelScope + nextRecursResult)

calcResistMain calcType _ (x:xs) | x == "(" =
    let sublist = getSubstrFor ")" xs
        innerSeriesScope = calcResistMain ESeries resistsData sublist
        nextRecursResult = calcResistMain calcType resistsData (drop (length sublist) xs )
    in innerSeriesScope + nextRecursResult

calcResistMain EParallel resistsData (x:xs) = 1.0 / 
    
--calcResistMain calcType resistsData (x:xs) =
 --   in if x == "[" then calcResistMain EParallel resistsData xs
  --  else if


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let numResists = read input_line :: Int
    rawResistsData <- readInput 0 numResists
    rawResistLinkStr <- getLine
    --calc
    -- hPutStrLn stderr "Debug messages..."

    putStrLn "Equivalent Resistance"
    return ()