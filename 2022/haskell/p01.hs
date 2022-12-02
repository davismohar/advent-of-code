import Data.List
import System.Environment
import Util

newtype CalorieList = CalorieList {calories :: [Int]} deriving (Show)

newtype Elf = Elf {caloriesCarried :: Int} deriving (Show, Eq, Ord)

calSum :: CalorieList -> Int
calSum calorieList = sum (calories calorieList)

main :: IO ()
main = getSolutions "P01" part1 part2 >>= \solution -> putStrLn solution

part1 :: String -> String
part1 input = show $ caloriesCarried $ maximum $ getElves input

parseCalorieLists :: [String] -> [CalorieList]
parseCalorieLists input = map mapStringsToCalorieList (splitBy "" input)

mapStringsToCalorieList :: [String] -> CalorieList
mapStringsToCalorieList input = CalorieList (map read input)

mapCaloriesListsToElves :: [CalorieList] -> [Elf]
mapCaloriesListsToElves = map (Elf . calSum)

getElves :: String -> [Elf]
getElves input = mapCaloriesListsToElves $ parseCalorieLists $ getLines input

part2 :: String -> String
part2 input = show $ sum $ toInts $ firstThree $ descendingSort $ getElves input

descendingSort = sortBy (flip compare)

firstThree :: [Elf] -> [Elf]
firstThree [] = []
firstThree [_] = []
firstThree [_, _] = []
firstThree (a : b : c : _) = [a, b, c]

toInts :: [Elf] -> [Int]
toInts = map caloriesCarried
