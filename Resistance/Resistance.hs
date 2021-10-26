import System.IO
import Control.Monad
import Data.List (elemIndex, find)

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

data CalcType = ESeries | EParallel deriving(Enum)

bracketToCalcType "[" = Just EParallel
bracketToCalcType "(" = Just ESeries
bracketToCalcType _ = Nothing
isOpenBracket bracket = bracket == "(" || bracket == "["
isCloseBracket bracket = bracket == ")" || bracket == "]"

getOpenBracketFunc mainFunc calcType resistData [] = (0.0, [])
getOpenBracketFunc mainFunc calcType resistData (x:xs) | x == "[" = calcResistMain EParallel resistData xs
getOpenBracketFunc mainFunc calcType resistData (x:xs) | x == "(" = calcResistMain ESeries resistData xs

getResistance :: String -> [(String, Float)] -> Float
getResistance resistor resistData =
    let Just result = find (\(resistName, resistance) -> resistName == resistor) resistData
    in snd result

calcResistMain :: CalcType -> [(String, Float)] -> [String] -> (Float, [String])
calcResistMain _ _ [] = (0.0, [])

calcResistMain _ _ (x:xs) | isCloseBracket x = (0.0, xs)

calcResistMain calcType resistData (x:xs) | isOpenBracket x =
    let Just innerCalcType = bracketToCalcType x
        (resistance, x2:xs2) = getOpenBracketFunc calcResistMain innerCalcType resistData (x:xs)
    in (resistance, xs2)


--calcResisMain 

--calcResistMain ESeries resistData (x:xs) = 
--    let recursiveResult = (getResistance x resistData xs)

{-
        innerParallelScope = calcResistMain EParallel resistsData sublist
        nextRecursResult = calcResistMain calcType resistsData (drop (length sublist) xs )
    in 1.0 / (innerParallelScope + nextRecursResult)-}
{-
calcResistMain calcType _ (x:xs) | x == "(" =
    let sublist = getSubstrFor ")" xs
        innerSeriesScope = calcResistMain ESeries resistsData sublist
        nextRecursResult = calcResistMain calcType resistsData (drop (length sublist) xs )
    in innerSeriesScope + nextRecursResult
-}
--calcResistMain EParallel resistsData (x:xs) = 1.0 / 

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