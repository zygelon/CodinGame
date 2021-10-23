import System.IO
import Control.Monad
import GHC.Num (signumInteger)

data Action = Action{ destDistance :: (Int, Int),
                      moveDirStr :: String } deriving Show
getThorRelDestPoint thorX thorY lightX lightY = (lightX - thorX, lightY - thorY)

getOutStr :: Action -> String 
getOutStr (Action _ moveStr) = moveStr 

getDestLoc :: Action -> (Int, Int)
getDestLoc (Action destLoc _) = destLoc

convertDirXToStr 1 = "W"
convertDirXToStr (-1) = "E"
convertDirXToStr 0 = ""
convertDirXToStr _ = "" --Just believe me

convertDirYToStr 1 = "N"
convertDirYToStr (-1) =  "S"
convertDirYToStr 0 =  ""
convertDirYToStr _ = "" 

(x1, y1) `pointPlus` (x2, y2) = (x1 + x2, y1 + y2)

(Action dest1 moveStr) `actionPlusLoc` dest2 = Action (dest1 `pointPlus` dest2) moveStr

aplus :: Action -> Action -> Action
(Action dest1 moveStr1) `aplus` (Action dest2 moveStr2) = 
  Action (dest1 `pointPlus` dest2)  (moveStr1 ++ moveStr2)

makeYAction :: Int -> Action
makeYAction yDir = 
  let nextYMove = negate (signum yDir)
  in Action (0, yDir + nextYMove) (convertDirYToStr nextYMove)

makeXAction :: Int -> Action
makeXAction xDir = 
  let nextXMove = negate (signum xDir)
  in Action (xDir + nextXMove, 0) (convertDirXToStr nextXMove)

calcNextAction :: (Int, Int) -> Action
calcNextAction destDir = makeYAction (snd destDir) `aplus` makeXAction (fst destDir)

loop thorDestPoint = do
  input_line <- getLine
  let newAction = calcNextAction thorDestPoint
  hPutStrLn stderr (show newAction)
  putStrLn $ getOutStr newAction
  loop $ getDestLoc newAction

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
    loop $ getThorRelDestPoint initialtx initialty lightx lighty