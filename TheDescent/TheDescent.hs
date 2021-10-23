import System.IO
import Control.Monad

main :: IO ()

mountainN = 8
{-
mointainReadLoop curMountain calcMax = do
            inputLine <- getLine
            if curMountain >= mountainN 
                then return calcMax
            
            let mountainh = read inputLine :: Int -- represents the height of one mountain.
            return ()
            --return mointainReadLoop curMountain max (calcMax mountainh)
-}
readInput :: IO [Int]
readInput = do
    lines <- replicateM (mountainN) $ do
        line <- getLine 
        let mountainh = read line :: Int
        return mountainh
    return lines

readMountains = do
    input <- getLine 
    let list = read input :: [Int]
    print list

myMax list = foldl max 0 list

main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- The while loop represents the game.
    -- Each iteration represents a turn of the game
    -- where you are given inputs (the heights of the mountains)
    -- and where you have to print an output (the index of the mountain to fire on)
    -- The inputs you are given are automatically updated according to your last actions.
    -- game loop
    forever $ do
--        let convertToInt x <- x
        inputArr <- readInput
        print $ myMax inputArr
        -- hPutStrLn stderr "Debug messages..."
        
        -- The index of the mountain to fire on.
