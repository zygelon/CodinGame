import System.IO
import Control.Monad
--001100000
--000100000 conway
setupLen = 9

data CellSetup = CellSetup { alive :: [Bool]
                           , dead  :: [Bool] }

newtype World = World [[Bool]]

strWCellToBool '.' = False
strWCellToBool 'O' = True

makeBoolFromStrWorld :: [String] -> [[Bool]]
makeBoolFromStrWorld (x:xs) = fmap strWCellToBool x : makeBoolFromStrWorld xs

strSetupToBool '1' = True
strSetupToBool '0' = False

inputToWSetup :: String -> String -> CellSetup
inputToWSetup alive dead | length alive == setupLen && length dead == setupLen =
    let aliveSetup = fmap strSetupToBool alive
        deadSetup = fmap strSetupToBool dead
    in CellSetup aliveSetup deadSetup

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let input = words input_line
    let h = read (input!!0) :: Int
    let w = read (input!!1) :: Int
    let n = read (input!!2) :: Int
    --dead -> alive
    alive <- getLine
    --alive -> dead
    dead <- getLine

    worldStr <- replicateM h $ do getLine

    let world = World $ makeBoolFromStrWorld worldStr
    -- hPutStrLn stderr "Debug messages..."

    -- Write answer to stdout
    putStrLn "grid"
    return ()