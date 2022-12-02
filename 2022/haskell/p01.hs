import Data.List
import System.Environment
import Util

newtype CalorieList = CalorieList {calories :: [Int]} deriving (Show)

newtype Elf = Elf {caloriesCarried :: Int} deriving (Show, Eq, Ord)

calSum :: CalorieList -> Int
calSum calorieList = sum (calories calorieList)

main :: IO ()
main = do
  result <- getSolutions "P01" part1 part2
  putStrLn result

part1 :: String -> String
part1 input = show $ caloriesCarried $ maximum $ mapCaloriesListsToElves (parseCalorieLists (getLines input))

parseCalorieLists :: [String] -> [CalorieList]
parseCalorieLists input = map mapStringsToCalorieList (splitBy "" input)

mapStringsToCalorieList :: [String] -> CalorieList
mapStringsToCalorieList input = CalorieList (map read input)

mapCaloriesListsToElves :: [CalorieList] -> [Elf]
mapCaloriesListsToElves = map (Elf . calSum)

part2 :: String -> String
part2 input = show $ sum $ toInts $ firstThree $ descendingSort $ mapCaloriesListsToElves $ parseCalorieLists $ getLines input

descendingSort = sortBy (flip compare)

firstThree :: [Elf] -> [Elf]
firstThree [] = []
firstThree [_] = []
firstThree [_, _] = []
firstThree (a : b : c : _) = [a, b, c]

toInts :: [Elf] -> [Int]
toInts = map caloriesCarried
