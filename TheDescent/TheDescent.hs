import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- The while loop represents the game.
    -- Each iteration represents a turn of the game
    -- where you are given inputs (the heights of the mountains)
    -- and where you have to print an output (the index of the mountain to fire on)
    -- The inputs you are given are automatically updated according to your last actions.
    
    
    -- game loop
    forever $ do
        
        replicateM 8 $ do
            input_line <- getLine
            let mountainh = read input_line :: Int -- represents the height of one mountain.
            return ()
        
        -- hPutStrLn stderr "Debug messages..."
        
        -- The index of the mountain to fire on.
        putStrLn "4"