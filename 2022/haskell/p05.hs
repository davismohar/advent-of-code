import Data.List
import Data.Maybe
import Data.Sequence (mapWithIndex)
import Debug.Trace
import Distribution.Simple.Utils (xargs)
import Text.Read (readMaybe)
import Util

type Crate = Char

type CrateStack = [Crate]

data CargoRoom = CargoRoom {crateStacks :: [CrateStack], size :: Int} deriving (Eq, Show)

data MoveCommand = MoveCommand {numToMove :: Int, fromStack :: Int, toStack :: Int} deriving (Eq, Show)

type Procedure = [MoveCommand]

data Instructions = Instructions {cargoRoom :: CargoRoom, procedure :: Procedure} deriving (Eq, Show)

data Mode = CRATE_MOVER_9000 | CRATE_MOVER_9001 deriving (Eq, Show, Enum)

main :: IO ()
main = getSolutions "P05" part1 part2 >>= putStrLn

part1 :: String -> String
part1 input = show $ getTopCrates $ cargoRoom $ evaluateInstructionsPart1 $ parseInstructions $ getLines input

parseInstructions :: [String] -> Instructions
parseInstructions lines = Instructions (parseCargoRoom lines) (parseProcedure lines)

getRoomWidth :: [String] -> Int
getRoomWidth lines = read $ last $ init (splitBy ' ' (getAxisLine lines))

getAxisLine :: [String] -> String
getAxisLine input = input !! getIndexOfXAxisLine input

getIndexOfXAxisLine :: [String] -> Int
getIndexOfXAxisLine lines = fromMaybe 0 (elemIndex True (map isAxisLine lines))

getGridLines :: [String] -> Int -> [String]
getGridLines lines xAxisLine = take xAxisLine lines

parseProcedure :: [String] -> Procedure
parseProcedure lines = map parseMoveCommand (getProcedureLines lines (getIndexOfXAxisLine lines))

parseMoveCommand :: String -> MoveCommand
parseMoveCommand line =
  let splitString = splitBy ' ' line
   in MoveCommand (read $ splitString !! 1) (read (splitString !! 3) -1) (read (splitString !! 5) -1)

getProcedureLines :: [String] -> Int -> [String]
getProcedureLines lines xAxisLine = drop (xAxisLine + 2) lines

parseCargoRoom :: [String] -> CargoRoom
parseCargoRoom lines = CargoRoom (parseCrateStacks (getGridLines lines (getIndexOfXAxisLine lines)) (getRoomWidth lines)) (getRoomWidth lines)

parseCrateStacks :: [String] -> Int -> [CrateStack]
parseCrateStacks lines size = map (parseCrateStack lines) [0 .. (size - 1)]

parseCrateStack :: [String] -> Int -> CrateStack
parseCrateStack lines index = stripSpaces $ map (!! (index * 4 + 1)) lines

stripSpaces :: String -> String
stripSpaces = filter (/= ' ')

isAxisLine :: String -> Bool
isAxisLine line
  | null line = False
  | line !! 1 == '1' = True
  | otherwise = False

getCrates :: String -> Int -> [Crate]
getCrates line size = map (line !!) [0 .. (size -1)]

evaluateInstructionsPart1 :: Instructions -> Instructions
evaluateInstructionsPart1 instructions = evaluateInstructions instructions CRATE_MOVER_9000

evaluateInstructions :: Instructions -> Mode -> Instructions
evaluateInstructions instructions@(Instructions _ procedure) mode
  | null procedure = instructions
  | otherwise = evaluateInstructions (getNextState instructions mode) mode

getNextState :: Instructions -> Mode -> Instructions
getNextState (Instructions room procedure) mode =
  Instructions (applyCommandToRoom (head procedure) room mode) (tail procedure)

applyCommandToRoom :: MoveCommand -> CargoRoom -> Mode -> CargoRoom
applyCommandToRoom cmd room mode = CargoRoom (applyCommandToStacks cmd (crateStacks room) mode) (size room)

applyCommandToStacks :: MoveCommand -> [CrateStack] -> Mode -> [CrateStack]
applyCommandToStacks cmd stacks mode =
  let movedCrates = take (numToMove cmd) (stacks !! fromStack cmd)
   in map (\istack -> applyCommandToStack cmd istack movedCrates mode) (index stacks)

index :: [CrateStack] -> [IndexedCrateStack]
index stacks = map (\i -> (stacks !! i, i)) [0 .. length stacks -1]

type IndexedCrateStack = (CrateStack, Int)

getNewStack :: CrateStack -> [Crate] -> Mode -> CrateStack
getNewStack stack movedCrates mode
  | mode == CRATE_MOVER_9000 = reverse movedCrates ++ stack
  | mode == CRATE_MOVER_9001 = movedCrates ++ stack

applyCommandToStack :: MoveCommand -> IndexedCrateStack -> [Crate] -> Mode -> CrateStack
applyCommandToStack cmd iStack movedCrates mode
  | i == fromStack cmd = drop (numToMove cmd) (fst iStack)
  | i == toStack cmd = getNewStack (fst iStack) movedCrates mode
  | otherwise = fst iStack
  where
    i = snd iStack

getTopCrates :: CargoRoom -> [Crate]
getTopCrates room = map head (crateStacks room)

part2 :: String -> String
part2 input = show $ getTopCrates $ cargoRoom $ evaluateInstructionsPart2 $ parseInstructions $ getLines input

evaluateInstructionsPart2 :: Instructions -> Instructions
evaluateInstructionsPart2 instructions = evaluateInstructions instructions CRATE_MOVER_9001