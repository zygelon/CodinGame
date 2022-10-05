import System.IO
import Control.Monad
import Data.Char

toBin list =
    let toBinImpl 0 = []
        toBinImpl n | odd n  = toBinImpl (n `div` 2) ++ ['1']
        toBinImpl n | even n = toBinImpl (n `div` 2) ++ ['0']
        binRes = toBinImpl list
        needZeros = 7 - length binRes
    in replicate needZeros '0' ++ binRes

toBinSeq :: String -> String
toBinSeq list = foldr ((++) . toBin . ord) [] list

data StreakType = Zero | One | Undefied deriving(Enum, Eq, Show)

charToStreak '0' = Zero
charToStreak '1' = One

convertToZeros :: StreakType -> String -> String
convertToZeros streakType [] = []

convertToZeros streakType (x:xs)                            | streakType == charToStreak x =
    '0' : convertToZeros streakType xs

convertToZeros streakType (x:xs)                            | (streakType == One || streakType == Undefied) && x == '0' =
    " 00 0" ++ convertToZeros Zero xs

convertToZeros streakType (x:xs)                            | (streakType == Zero || streakType == Undefied) && x == '1' =
    " 0 0" ++ convertToZeros One xs

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    asciiChars <- getLine
    putStrLn $ tail $ convertToZeros Undefied $ toBinSeq asciiChars
