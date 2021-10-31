import System.IO
import Control.Monad

vspeedLim = 40.0
gravi = 3.711
--data InputData = InputData {}
--calculatePower :: Float -> Float -> Float -> (Int, Float)
calcTimeToEarth targy vspeed accel = 
    let descr = 4.0 * vspeed * vspeed + (8.0 * targy * accel)
        t2 = ((-2.0 * vspeed) - sqrt descr) / (2.0 * accel)
    in t2

--calculatePower :: Num a => Float -> Float -> p -> (a, Float)
calculatePower targy vspeed fuel = 
    let powerTime = calcTimeToEarth targy vspeed (gravi - 4)
    in if abs (powerTime / 4.0) < fuel 
       then (4, powerTime)
       else (0, powerTime)


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let surfacen = read input_line :: Int -- the number of points used to draw the surface of Mars.
    
    replicateM surfacen $ do
        input_line <- getLine
        let input = words input_line
        let landx = read (input!!0) :: Int -- X coordinate of a surface point. (0 to 6999)
        let landy = read (input!!1) :: Int -- Y coordinate of a surface point. By linking all the points together in a sequential fashion, you form the surface of Mars.
        return ()
    
    -- game loop
    forever $ do
        input_line <- getLine
        let input = words input_line
        let x = read (input!!0) :: Int
        let y = read (input!!1) :: Float
        let hspeed = read (input!!2) :: Int -- the horizontal speed (in m/s), can be negative.
        let vspeed = read (input!!3) :: Float -- the vertical speed (in m/s), can be negative.
        let fuel = read (input!!4) :: Float -- the quantity of remaining fuel in liters.
        let rotate = read (input!!5) :: Int -- the rotation angle in degrees (-90 to 90).
        let power = read (input!!6) :: Int -- the thrust power (0 to 4).
        
        -- hPutStrLn stderr "Debug messages..."
        let (res, powerTime) = calculatePower y vspeed fuel
        hPutStrLn stderr $ show powerTime
        --hPutStrLn stderr $ show t1
        --hPutStrLn stderr $ show t2
        -- 2 integers: rotate power. rotate is the desired rotation angle (should be 0 for level 1), power is the desired thrust power (0 to 4).
        putStrLn $ "0 " ++ show res
       -- putStrLn "0 3"