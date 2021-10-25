import System.IO
import Control.Monad

readInput :: Int -> Int -> IO [String]
readInput currResist numResists = do
    if currResist >= numResists
        then return []
        else do
            line <- getLine
            recursiveResult <- readInput (currResist + 1) numResists
            return (line : recursiveResult)

parseResistorsData :: [String] -> [(String, Int)]
parseResistorsData [] = []
parseResistorsData (x:xs) = 
    let resistorName = head $ words x
        resistorPower = read (last $ words x) :: Int
    in (resistorName, resistorPower) : parseResistorsData xs


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let numResists = read input_line :: Int
    rawResistsData <- readInput 0 numResists
    
    -- hPutStrLn stderr "Debug messages..."
    
    putStrLn "Equivalent Resistance"
    return ()