import Control.Arrow (Arrow (second))
import Data.Set (Set, empty, insert, singleton, size)
import Util

main :: IO ()
main = getSolutions "P09" part1 part2 >>= putStrLn

part1 :: String -> String
part1 input = show $ size $ visitedCoords $ applyDirections initialGrid $ parseDirections $ getLines input

part2 :: String -> String
part2 input = "part2"

type Coordinate = (Int, Int)

data GridState = GridState {headPos :: Coordinate, tailPos :: Coordinate, visitedCoords :: Set Coordinate} deriving (Eq, Show)

data GridState1 = GridState1 {rope :: [Coordinate], visited :: [Set Coordinate]} deriving (Eq, Show)

initialGrid = GridState (0, 0) (0, 0) (singleton (0, 0))

data Direction = L | R | U | D | UNKNOWN deriving (Enum, Show, Eq)

parseDirectionFromChar :: Char -> Direction
parseDirectionFromChar char
  | char == 'L' = L
  | char == 'R' = R
  | char == 'U' = U
  | char == 'D' = D

parseDirection :: String -> [Direction]
parseDirection [] = []
parseDirection [_] = []
parseDirection [_, _] = []
parseDirection (dirChar : _ : numString) = replicate (read numString) (parseDirectionFromChar dirChar)

parseDirections :: [String] -> [Direction]
parseDirections [] = []
parseDirections [line] = parseDirection line
parseDirections (line : rest) = parseDirection line ++ parseDirections rest

applyDirection :: Direction -> GridState -> GridState
applyDirection dir gState = GridState (newHeadPos (dir) gState) (newTailPos dir gState) (insert (newTailPos dir gState) (visitedCoords gState))

applyDirections :: GridState -> [Direction] -> GridState
applyDirections = foldl (flip applyDirection)

newTailPos :: Direction -> GridState -> Coordinate
newTailPos dir gState
  | isAdjacent (newHeadPos dir gState) (tailPos gState) = tailPos gState
  | otherwise = moveTailToTouch dir (newHeadPos dir gState)

moveTailToTouch :: Direction -> Coordinate -> Coordinate
moveTailToTouch dir hPos
  | dir == L = rightOne hPos
  | dir == R = leftOne hPos
  | dir == U = downOne hPos
  | dir == D = upOne hPos

isAdjacent :: Coordinate -> Coordinate -> Bool
isAdjacent (x1, y1) (x2, y2) = isWithinOne x1 x2 && isWithinOne y1 y2

isWithinOne :: Int -> Int -> Bool
isWithinOne a b = abs (a - b) <= 1

newHeadPos :: Direction -> GridState -> Coordinate
newHeadPos dir (GridState head _ _)
  | dir == L = leftOne head
  | dir == R = rightOne head
  | dir == U = upOne head
  | dir == D = downOne head

leftOne :: Coordinate -> Coordinate
leftOne (x, y) = (x -1, y)

rightOne :: Coordinate -> Coordinate
rightOne (x, y) = (x + 1, y)

upOne :: Coordinate -> Coordinate
upOne (x, y) = (x, y + 1)

downOne :: Coordinate -> Coordinate
downOne (x, y) = (x, y -1)