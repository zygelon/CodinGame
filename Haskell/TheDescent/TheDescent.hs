import System.IO
import Control.Monad

main :: IO ()

mountainN = 8

readInput :: Int -> IO [(Int, Int)]
readInput currEpoch = do
    if currEpoch >= mountainN
        then return []
        else do
            line <- getLine
            let mountainVar = (read line :: Int, currEpoch)
            result <- readInput $ currEpoch + 1
            return (mountainVar : result)

myMax :: (Int, Int) -> (Int, Int) -> (Int, Int)
myMax (height1, num1) (height2, num2) | height2 > height1 = (height2, num2)
myMax moutain _ =  moutain

myMaxFold = foldl myMax (-1, -1)

showStrTuple :: (Show a1, Show a2) => (a1, a2) -> [Char]
showStrTuple (height, currMountain) = "(" ++ show height ++ ", " ++ show currMountain ++ ") "

showStrList :: [(Int, Int)] -> String
showStrList xs = foldr ((++) . showStrTuple) "" xs

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- The while loop represents the game.
    -- Each iteration represents a turn of the game
    -- where you are given inputs (the heights of the mountains)
    -- and where you have to print an output (the index of the mountain to fire on)
    -- The inputs you are given are automatically updated according to your last actions.
    -- game loop
    forever $ do
        inputArr <- readInput 0
        hPutStrLn stderr "Debug Out Start"
        hPutStrLn stderr (showStrList inputArr)
        print $ snd (myMaxFold inputArr)
        -- hPutStrLn stderr "Debug messages..."

        -- The index of the mountain to fire on.
