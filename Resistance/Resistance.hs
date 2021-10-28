{-# LANGUAGE MultiWayIf #-}
import System.IO
import Control.Monad
import Data.Either
import Data.List (elemIndex, find)
import Numeric
import Text.Printf
{-readInput :: Int -> Int -> IO [String]
readInput currResist numResists = do
    if currResist >= numResists
        then return []
        else do
            line <- getLine
            recursiveResult <- readInput (currResist + 1) numResists
            return (line : recursiveResult)-}

convertRawToResistData :: [String] -> [(String, Float)]
convertRawToResistData [] = []
convertRawToResistData (x:xs) = 
    let (resistName, resistFloat) = (words x !! 0, words x !! 1)
    in (resistName, read resistFloat :: Float) : convertRawToResistData xs

data CalcType = ESeries | EParallel deriving(Enum, Eq)

bracketToCalcType "[" = Just EParallel
bracketToCalcType "(" = Just ESeries
bracketToCalcType _ = Nothing
isOpenBracket bracket = bracket == "(" || bracket == "["
isCloseBracket bracket = bracket == ")" || bracket == "]"

getOpenBracketFunc mainFunc calcType resistData [] = Nothing --(0.0, [])
getOpenBracketFunc mainFunc calcType resistData (x:xs) | x == "[" = calcResistMain EParallel resistData xs
getOpenBracketFunc mainFunc calcType resistData (x:xs) | x == "(" = calcResistMain ESeries resistData xs


getResistance :: String -> [(String, Float)] -> Float
getResistance resistorName resistData =
    let Just result = find (\(lambResistName, resistance) -> lambResistName == resistorName) resistData
    in snd result


calcResistMain :: CalcType -> [(String, Float)] -> [String] -> Maybe (Float, [String])
calcResistMain _ _ [] = Nothing--Just (0.0, [])

calcResistMain calcType _ (x:xs) | x == ")" && calcType == ESeries = Just (0.0, xs)
calcResistMain calcType _ (x:xs) | x == "]" && calcType == EParallel =  Just (0.0, xs)
calcResistMain _ _ (x:xs) | isCloseBracket x = Nothing

calcResistMain calcType resistData (x:xs) | isOpenBracket x =
    let Just innerCalcType = bracketToCalcType x
        Just (resistFloat, list) = getOpenBracketFunc calcResistMain innerCalcType resistData (x:xs)
        --resistFloat = getResistance resistance resistData
        resistFloatCalcTyped = if innerCalcType == ESeries then resistFloat else 1.0 / resistFloat
        Just (nextResistance, list2) = calcResistMain calcType resistData list
    in Just ((resistFloatCalcTyped + nextResistance), list2)

calcResistMain calcType resistData (x:xs) =
    let resistFloat = getResistance x resistData
        Just (nextResistance, list) = calcResistMain calcType resistData xs
        calcTypeResistance = if calcType == ESeries then resistFloat else 1.0 / resistFloat
    in Just ((calcTypeResistance + nextResistance), list)

activate :: [(String, Float)] -> [String] -> Maybe (Float, [String])
activate resistData (x:xs) =
    if | x == "[" = calcResistMain EParallel resistFloat xs 
       | x == "(" = calcResistMain ESeries resistFloat xs
       | otherwise = Nothing

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let numResists = read input_line :: Int
    rawResistsData <- replicateM numResists getLine
    rawResistLinkStr <- getLine
    --print $ show rawResistsData
    --print $ show rawResistLinkStr
    --print "______________________"
    let resistsData = convertRawToResistData rawResistsData
    --print $ show resistsData
    let Just (result, _) = activate resistsData (words rawResistLinkStr)
    -- hPutStrLn stderr "Debug messages..."
    let outStr _ = showFFloat (Just 10) result
    printf "%.1f\n" $ result
    --putStrLn "Equivalent Resistance"
{- Test
2
A 24
C 48
[ A C ]
-------------------
1/(1/48+1/24) == 16
-}