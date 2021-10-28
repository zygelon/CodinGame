{-# LANGUAGE MultiWayIf #-}
import System.IO
import Control.Monad
import Data.List (elemIndex, find)
import Debug.Trace
import Text.Printf

data CalcType = ESeries | EParallel deriving(Enum, Show)
data RetRecurs = RetRecurs Float [String] --deriving(Show)
--data 
toStr :: [String] -> String
toStr [] = ""
toStr (x:xs) = x ++ " " ++ toStr xs

instance Show RetRecurs where
    show (RetRecurs fData sData) = show fData ++ " | " ++ toStr sData

convertRawToResistData :: [String] -> [(String, Float)]
convertRawToResistData [] = []
convertRawToResistData (x:xs) =
    let (resistName, resistFloat) = (words x !! 0, words x !! 1)
    in (resistName, read resistFloat :: Float) : convertRawToResistData xs

getResistFromStr :: [(String, Float)] -> String -> Float
getResistFromStr resistsData resistorName = 
    let Just (foundName, fResist) = find (\(lambResistName, resistance) -> lambResistName == resistorName) resistsData
    in fResist

myShow :: Show a => String -> a -> a
--myShow debugInfo a | trace ("myShow " ++ debugInfo ++ " = "  ++ show a) False = undefined 
myShow debugInfo a = a

calcResult :: (String -> Float) -> [String] -> CalcType -> RetRecurs
--calcResult sgetResist (x:xs) calcType | trace ("calcResult " ++ toStr (x:xs) ++ " " ++ show calcType) False = undefined 
calcResult _ (x:xs) EParallel | x == "]" = RetRecurs 0.0 xs
calcResult _ (x:xs) ESeries   | x == ")" = RetRecurs 0.0 xs
calcResult _ (x:xs) _  | x == ")" || x == "]" = RetRecurs 10000000.0 ["PIZDA"]
calcResult sgetResist (x:xs) calcType = 
    let (RetRecurs fRet sRet) = getResistRecur sgetResist (x:xs) calcType
        (RetRecurs fnRet snRet) = calcResult sgetResist sRet calcType
        temp = myShow "calcResult" (RetRecurs (fRet + fnRet) snRet)
    in temp--RetRecurs (fRet + fnRet) snRet



getResistRecur :: (String -> Float) -> [String] -> CalcType -> RetRecurs
--getResistRecur sgetResist (x:xs) calcType | trace ("getResistRecur " ++ toStr (x:xs) ++ " " ++ show calcType) False = undefined 
getResistRecur sgetResist (x:xs) EParallel | x == "[" = calcResult sgetResist xs EParallel
getResistRecur sgetResist (x:xs) ESeries | x == "[" = 
    let (RetRecurs fRet  sRes) = calcResult sgetResist xs EParallel
        temp = myShow "getResistRecur" (RetRecurs (1.0 / fRet) sRes)
    in temp--RetRecurs (1.0 / fRet) sRes
getResistRecur sgetResist (x:xs) ESeries | x == "(" = calcResult sgetResist xs ESeries
getResistRecur sgetResist (x:xs) EParallel | x == "(" = 
    let (RetRecurs fRes sRes) = calcResult sgetResist xs ESeries
    in RetRecurs (1.0 / fRes) sRes

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
    let resistsData = convertRawToResistData rawResistsData
    let getResistByName = getResistFromStr resistsData
    let (RetRecurs fResult sResult) = getResistRecur getResistByName (words rawResistLinkStr) ESeries
    printf "%.1f\n" fResult