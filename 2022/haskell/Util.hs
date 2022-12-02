module Util
  ( splitBy,
    parseFile,
    getLines,
    getSolutions,
  )
where

import System.Environment
import System.IO

splitBy :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitBy delimiter = foldr f [[]]
  where
    f _ [] = [[]]
    f curr all@(building : handled)
      | curr == delimiter = [] : all
      | otherwise = (curr : building) : handled

parseFile :: String -> IO String
parseFile filename = do
  handle <- openFile filename ReadMode
  hGetContents handle

getNonEmptyLines :: String -> [String]
getNonEmptyLines filename = stripEmptyLines $ getLines filename

getLines :: String -> [String]
getLines = splitBy '\n'

stripEmptyLines :: [String] -> [String]
stripEmptyLines = filter (/= "")

buildResultString :: String -> String -> String -> String
buildResultString puzzleName part1Result part2Result =
  puzzleName ++ "\n"
    ++ "------\n"
    ++ "part1:\n"
    ++ part1Result
    ++ "\n"
    ++ "------\n"
    ++ "part2:\n"
    ++ part2Result
    ++ "\n"
    ++ "------"

getSolutions :: String -> (String -> String) -> (String -> String) -> IO String
getSolutions puzzle part1 part2 = do
  args <- System.Environment.getArgs
  let mode = getMode args
  contents <- getFileContents puzzle mode
  return $ buildResultString puzzle (part1 contents) (part2 contents)

getMode :: [String] -> String
getMode [] = "input"
getMode (mode : _) = mode

getFileContents :: String -> String -> IO String
getFileContents puzzle mode
  | mode == "test" = parseFile ("input/" ++ puzzle ++ "/test.txt")
  | otherwise = parseFile ("input/" ++ puzzle ++ "/input.txt")
