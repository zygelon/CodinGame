import System.IO
import Control.Monad
import GHC.Num (signumInteger)

data Action = Action{ destDistance :: (Int, Int),
                      moveDirStr :: String } deriving Show
getThorRelDestPoint thorX thorY lightX lightY = (lightX - thorX, lightY - thorY)

getMainOutStr :: Action -> String 
getMainOutStr (Action _ moveStr) = moveStr 

convertDirXToStr 1 = "W"
convertDirXToStr (-1) = "E"
convertDirXToStr 0 = ""
convertDirXToStr _ = "" --Just believe me

convertDirYToStr 1 = "N"
convertDirYToStr (-1) =  "S"
convertDirYToStr 0 =  ""
convertDirYToStr _ = "" 

aplus :: Action -> Action -> Action
(Action dest1 movedir1) `aplus` (Action dest2 movedir2) = 
  Action (fst dest1 + fst dest2, snd dest1 + snd dest2)  (movedir1 ++ movedir2)
  

makeYAction :: Int -> Action
makeYAction yDir = 
  let nextYMove = negate (signum yDir)
  in Action (0, yDir + nextYMove) (convertDirYToStr nextYMove)

makeXAction :: Int -> Action
makeXAction xDir = 
  let nextXMove = negate (signum xDir)
  in Action (xDir + nextXMove, 0) (convertDirXToStr nextXMove)

calcNextAction :: Action -> Action
calcNextAction (Action destDir dirStr) = makeYAction (snd destDir) `aplus` makeXAction (fst destDir)

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
    
    -- game loop
    forever $ do
        --Ignore
        input_line <- getLine
        let remainingTurns = read input_line :: Int -- The remaining amount of turns Thor can move. Do not remove this line.
        let thorDestPoint = getThorRelDestPoint initialtx initialty lightx lighty
        let action = calcNextAction (Action thorDestPoint "")
        hPutStrLn stderr (show action)
        putStrLn $ getMainOutStr action
        
        -- hPutStrLn stderr "Debug messages..."
        
        -- A single line providing the move to be made: N NE E SE S SW W or NW
        
        --putStrLn "SE"