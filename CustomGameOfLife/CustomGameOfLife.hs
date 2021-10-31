import System.IO
import Control.Monad
--NEED POLISHING THIS FORMAT
--012345678
--001100000
--000100000 conway
setupLen = 9

data CellSetup = CellSetup { alive :: [Bool]
                           , dead  :: [Bool] }

type World = [[Bool]]

strWCellToBool '.' = False
strWCellToBool 'O' = True
boolWCellToStr True  = 'O'
boolWCellToStr False = '.'


makeBoolFromStrWorld :: [String] -> World
makeBoolFromStrWorld list = map (map strWCellToBool) list

makeStrFromBoolWorld :: World -> [String]
makeStrFromBoolWorld list = map (map boolWCellToStr) list

strSetupToBool '1' = True
strSetupToBool '0' = False

inputToWSetup :: String -> String -> CellSetup
inputToWSetup alive dead | length alive == setupLen && length dead == setupLen =
    let aliveSetup = map strSetupToBool alive
        deadSetup = map strSetupToBool dead
    in CellSetup aliveSetup deadSetup

getCell :: World -> Int -> Int -> Bool
getCell _ x y                                          | x < 0 || y < 0 = False
getCell world x y       | y >= length world || x >= length (world !! 0) = False
getCell world x y                                                       = world !! y !! x

getSumAliveNeighbours :: World -> Int -> Int -> Int
getSumAliveNeighbours world cellX cellY =
    let neighbors = [[-1, -1], --0
                     [-1,  0], --1
                     [-1,  1], --2
                     [ 0, -1], --3
                     [ 0,  1], --4
                     [ 1, -1], --5
                     [ 1,  0], --6
                     [ 1,  1]] --7
        cellStatus = \[rely, relx] -> getCell world (cellX + relx) (cellY + rely)
        neighbStates = fmap cellStatus neighbors
    in foldl (\bFirstVal bSecondVal -> fromEnum bFirstVal + fromEnum bSecondVal) 0 neighbStates

getCellNextGeneration :: CellSetup -> World -> Int -> Int -> Bool
getCellNextGeneration (CellSetup alive dead) world cellX cellY =
    let aliveNeighbors = getSumAliveNeighbours world cellX cellY
        curCellStatus = world !! cellY !! cellX
    in if curCellStatus
       then dead !! aliveNeighbors -- alive -> dead transition ?
       else alive !! aliveNeighbors -- dead -> alive transition ?

getWorldNextGeneration :: CellSetup -> World -> World
getWorldNextGeneration cellSetup world =
    let height = length world
        width = length $ head world
        wNextGenAllLines :: Int -> World
        wNextGenAllLines y | y >= height = []
        wNextGenAllLines y               =
            let wNextGenLine :: Int -> Int -> [Bool]
                wNextGenLine x y =
                    let result = getCellNextGeneration cellSetup world x y
                    in if x < width then result : wNextGenLine (x + 1) y
                       else []
            in wNextGenLine 0 y : wNextGenAllLines (y + 1)
    in wNextGenAllLines 0

getWorldNGeneration :: CellSetup -> World -> Int -> World
getWorldNGeneration cellSetup world n =
    let getWorldNGenerationRec :: World -> Int -> World
        getWorldNGenerationRec currWorld currIter | currIter >= n = currWorld
        getWorldNGenerationRec currWorld currIter =
            let nextWGener = getWorldNextGeneration cellSetup world
            in getWorldNGenerationRec nextWGener (currIter + 1)
    in getWorldNGenerationRec world 0

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

    let cellSetup = inputToWSetup alive dead
    let world = makeBoolFromStrWorld worldStr
    let nGeneration = getWorldNGeneration cellSetup world n
    let strOutWorld = makeStrFromBoolWorld nGeneration
    let outFormatWGeneration = foldr (++) "" (map (\str -> str ++ "\n") strOutWorld)
    -- hPutStrLn stderr "Debug messages..."
    hPutStrLn stderr $ show world
    let debugWorld = getWorldNextGeneration cellSetup world
    hPutStrLn stderr "height"
    hPutStrLn stderr $ show (length world)
    hPutStrLn stderr "width"
    hPutStrLn stderr $ show (length $ head world)
    hPutStrLn stderr "debugWorld"
    hPutStrLn stderr $ show debugWorld
    hPutStrLn stderr (show nGeneration)
    -- Write answer to stdout
    putStrLn outFormatWGeneration
    return ()