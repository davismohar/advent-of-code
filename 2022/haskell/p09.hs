import Control.Arrow (Arrow (second))
import Data.Set (Set, empty, insert, singleton, size)
import Util

main :: IO ()
main = getSolutions "P09" part1 part2 >>= putStrLn

part1 :: String -> String
part1 input = show $ size $ visitedCoords $ applyDirections initialGrid $ parseDirections $ getLines input

part2 :: String -> String
part2 input = show $ size $ (!! 9) $ visited $ applyDirections1 (initialGrid1 10) $ parseDirections $ getLines input

type Coordinate = (Int, Int)

data GridState = GridState {headPos :: Coordinate, tailPos :: Coordinate, visitedCoords :: Set Coordinate} deriving (Eq, Show)

data GridState1 = GridState1 {rope :: [Coordinate], visited :: [Set Coordinate]} deriving (Eq, Show)

data Move = Move {previousPos :: Coordinate, newPos :: Coordinate}

initialGrid = GridState (0, 0) (0, 0) (singleton (0, 0))

initialGrid1 size = GridState1 (replicate size (0, 0)) (replicate size (singleton (0, 0)))

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

applyDirections1 :: GridState1 -> [Direction] -> GridState1
applyDirections1 = foldl applyDirection1

applyDirection1 :: GridState1 -> Direction -> GridState1
applyDirection1 gState dir = GridState1 (moveRope (rope gState) (dir)) (getNewVisitedPositions (moveRope (rope gState) (dir)) (visited gState))

moveRope :: [Coordinate] -> Direction -> [Coordinate]
moveRope coords dir = map newPos (getRopeMovements coords dir)

getNewVisitedPositions :: [Coordinate] -> [Set Coordinate] -> [Set Coordinate]
getNewVisitedPositions = zipWith insert

getRopeMovements :: [Coordinate] -> Direction -> [Move]
getRopeMovements coords dir = foldl (\acc coord -> acc ++ [newTailPos1 (last acc) coord]) [newHeadPos1 dir (head coords)] (tail coords)

newTailPos1 :: Move -> Coordinate -> Move
newTailPos1 headMove follow
  | isAdjacent (newPos headMove) follow = Move follow follow
  | isOnAxis (newPos headMove) follow = moveOnAxisToBeAdjacent (newPos headMove) follow
  | otherwise = moveDiagonallyToBeAdjacent (newPos headMove) follow

isOnAxis :: Coordinate -> Coordinate -> Bool
isOnAxis headCoord tailCoord = fst headCoord == fst tailCoord || snd headCoord == snd tailCoord

moveOnAxisToBeAdjacent :: Coordinate -> Coordinate -> Move
moveOnAxisToBeAdjacent headCoord tailCoord
  | fst headCoord > fst tailCoord = Move tailCoord (fst tailCoord + 1, snd tailCoord)
  | fst headCoord < fst tailCoord = Move tailCoord (fst tailCoord - 1, snd tailCoord)
  | snd headCoord > snd tailCoord = Move tailCoord (fst tailCoord, snd tailCoord + 1)
  | otherwise = Move tailCoord (fst tailCoord, snd tailCoord - 1)

moveDiagonallyToBeAdjacent :: Coordinate -> Coordinate -> Move
moveDiagonallyToBeAdjacent headCoord curCoord
  | fst headCoord > fst curCoord && snd headCoord > snd curCoord = Move curCoord (fst curCoord + 1, snd curCoord + 1)
  | fst headCoord > fst curCoord && snd headCoord < snd curCoord = Move curCoord (fst curCoord + 1, snd curCoord - 1)
  | fst headCoord < fst curCoord && snd headCoord > snd curCoord = Move curCoord (fst curCoord - 1, snd curCoord + 1)
  | otherwise = Move curCoord (fst curCoord - 1, snd curCoord - 1)

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

newHeadPos1 :: Direction -> Coordinate -> Move
newHeadPos1 dir coord
  | dir == L = Move coord (leftOne coord)
  | dir == R = Move coord (rightOne coord)
  | dir == U = Move coord (upOne coord)
  | dir == D = Move coord (downOne coord)

leftOne :: Coordinate -> Coordinate
leftOne (x, y) = (x -1, y)

rightOne :: Coordinate -> Coordinate
rightOne (x, y) = (x + 1, y)

upOne :: Coordinate -> Coordinate
upOne (x, y) = (x, y + 1)

downOne :: Coordinate -> Coordinate
downOne (x, y) = (x, y -1)