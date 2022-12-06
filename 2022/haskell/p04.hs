import Data.Foldable
import Util

main :: IO ()
main = getSolutions "P04" part1 part2 >>= putStrLn

data Assignment = Assignment {firstId :: Int, lastId :: Int}

instance Show Assignment where
  show a = (map (isInAssignment a) [0 .. 100]) ++ " " ++ show (firstId a) ++ "-" ++ show (lastId a)

isInAssignment (Assignment min1 max1) num
  | num >= min1 && num <= max1 = '_'
  | otherwise = ' '

data AssignmentPair = AssignmentPair {firstAssignment :: Assignment, secondAssignment :: Assignment}

instance Show AssignmentPair where
  show a = "\n" ++ show (firstAssignment a) ++ "\n" ++ show (secondAssignment a)

part1 :: String -> String
part1 input = show $ sum $ map (fromEnum . containsEncapsulatingAssignments . parseAssignmentPair) $ getLines input

parseAssignmentPair :: String -> AssignmentPair
parseAssignmentPair line = getAssignmentPair $ map parseAssignment (splitBy ',' line)

parseAssignment :: String -> Assignment
parseAssignment line = Assignment (read (head contents)) (read (last contents))
  where
    contents = splitBy '-' line

getAssignmentPair :: [Assignment] -> AssignmentPair
getAssignmentPair (a : b : _) = AssignmentPair a b

containsEncapsulatingAssignments :: AssignmentPair -> Bool
containsEncapsulatingAssignments (AssignmentPair a1 a2) = encapsulates a1 a2 || encapsulates a2 a1

encapsulates :: Assignment -> Assignment -> Bool
encapsulates (Assignment min1 max1) (Assignment min2 max2) = min1 <= min2 && max2 <= max1

-- 961 is too high
part2 :: String -> String
part2 input = show $ sum $ map (fromEnum . traced . containsOverlappingAssignments . traced . parseAssignmentPair) $ getLines input

containsOverlappingAssignments :: AssignmentPair -> Bool
containsOverlappingAssignments pair@(AssignmentPair a1 a2) = overlaps a1 a2 || containsEncapsulatingAssignments pair

overlaps :: Assignment -> Assignment -> Bool
overlaps (Assignment min1 max1) (Assignment min2 max2) = (max1 >= min2 && max1 <= max2) || (min1 >= min2 && min1 <= max2)
