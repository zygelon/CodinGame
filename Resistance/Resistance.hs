{-# LANGUAGE MultiWayIf #-}
import System.IO
import Control.Monad
import Data.List (elemIndex, find)

data CalcType = ESeries | EParallel deriving(Enum)
data RetRecurs = RetRecurs Float [String]
--data 

convertRawToResistData :: [String] -> [(String, Float)]
convertRawToResistData [] = []
convertRawToResistData (x:xs) =
    let (resistName, resistFloat) = (words x !! 0, words x !! 1)
    in (resistName, read resistFloat :: Float) : convertRawToResistData xs

getResistFromStr :: [(String, Float)] -> String -> Float
getResistFromStr resistsData resistorName = 
    let Just (foundName, fResist) = find (\(lambResistName, resistance) -> lambResistName == resistorName) resistsData
    in fResist

calcResult :: (String -> Float) -> [String] -> CalcType -> RetRecurs
calcResult _ (x:xs) EParallel | x == "]" = RetRecurs 0.0 xs
calcResult _ (x:xs) ESeries   | x == ")" = RetRecurs 0.0 xs
calcResult _ (x:xs) _  | x == ")" || x == "]" = RetRecurs 10000000.0 ["PIZDA"]
calcResult sgetResist (x:xs) calcType = 
    let (RetRecurs fRet sRet) = getResistRecur sgetResist (x:xs) calcType
        (RetRecurs fnRet snRet) = calcResult sgetResist sRet calcType
    in RetRecurs (fRet + fnRet) snRet

getResistRecur :: (String -> Float) -> [String] -> CalcType -> RetRecurs
getResistRecur sgetResist (x:xs) calcType | x == "[" = 
    let (RetRecurs fRet  sRes) = calcResult sgetResist xs EParallel
    in RetRecurs (1.0 / fRet) sRes
getResistRecur sgetResist (x:xs) calcType | x == "(" = calcResult sgetResist xs ESeries

getResistRecur sgetResist (x:xs) ESeries = RetRecurs (sgetResist x) xs
getResistRecur sgetResist (x:xs) EParallel = RetRecurs (1.0 / sgetResist x) xs

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    --quickCheck testA example
    input_line <- getLine
    let numResists = read input_line :: Int
    rawResistsData <- replicateM numResists getLine
    rawResistLinkStr <- getLine
    print $ show rawResistsData
    print $ show rawResistLinkStr
    print "______________________"
    let resistsData = convertRawToResistData rawResistsData
    let getResistByName = getResistFromStr resistsData
    let (RetRecurs fResult sResult) = getResistRecur getResistByName (words rawResistLinkStr) ESeries
    print ("fResult = " ++ show fResult)
    print ("sResult = " ++ show sResult)