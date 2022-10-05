import System.IO
import Control.Monad

loop :: Int -> Int -> IO()
loop x y = do 
  input_line <- getLine
  let xOut x | x > 0 = "E"
             | x < 0 = "W"
             | x == 0 = ""
  let yOut y | y > 0 = "S"
             | y < 0 = "N"
             | y == 0 = ""
  hPutStrLn stderr $ "x = " ++ show x ++ " y = " ++ show y
  hPutStrLn stderr $ "x = " ++ xOut x ++ " y = " ++ yOut y
  putStrLn (yOut y ++ xOut x)
  let nextLoc arg = arg - signum arg
  loop (nextLoc x) (nextLoc y)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    -- ---
    -- Hint: You can use the debug stream to print initialTX and initialTY, if Thor seems not follow your orders.
    
    input_line <- getLine
    let input = words input_line
    let lightx = read (input!!0) :: Int -- the X position of the light of power
    let lighty = read (input!!1) :: Int -- the Y position of the light of power
    let initialtx = read (input!!2) :: Int -- Thor's starting X position
    let initialty = read (input!!3) :: Int -- Thor's starting Y position
    loop  (lightx - initialtx) (lighty - initialty)
