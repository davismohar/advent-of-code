import Data.Map (Map, findWithDefault, fromList, insert, keys, lookup)
import Data.Maybe (fromMaybe)
import Distribution.Simple.Utils (currentDir)
import System.Posix (FileStatus)
import Util
import Prelude hiding (lookup)

main :: IO ()
main = getSolutions "P07" part1 part2 >>= putStrLn

part1 :: String -> String
part1 input = show $ sum $ getKeysLessThanEqual 100000 $ getDirSizes $ buildTerminalStateFrom input

part2 :: String -> String
part2 input = show $ sizeOfDirectoryToDeleteToMakeUpdateSpace $ buildTerminalStateFrom input

data File = File {size :: Int, filename :: String} deriving (Eq, Show)

data Directory = Directory {parentDir :: String, subDirs :: [String], files :: [File], dirname :: String} deriving (Eq, Show)

root = Directory "" [] [] "/"

emptyFs = fromList [("/", root)]

initTerminal = Terminal emptyFs "/"

totalDiskSpace = 70000000

updateSpaceRequired = 30000000

data Step = Step {command :: String, output :: [String]} deriving (Eq, Show)

buildTerminalStateFrom :: String -> Terminal
buildTerminalStateFrom input = applySteps initTerminal $ map parseStep $ drop 1 $ map getLines $ splitBy '$' input

parseStep :: [String] -> Step
parseStep lines = Step (tail $ head lines) (tail lines)

applySteps :: Terminal -> [Step] -> Terminal
applySteps term steps
  | null steps = term
  | otherwise = applySteps (applyStep term (head steps)) (tail steps)

applyStep :: Terminal -> Step -> Terminal
applyStep term step
  | command step == "ls" = applyLs term step
  | otherwise = applyCd term step

applyCd :: Terminal -> Step -> Terminal
applyCd term step
  | path == ".." = Terminal (filesystem term) (backOneDir $ currentDirectory term)
  | path == "/" = Terminal (filesystem term) ("/")
  | otherwise = Terminal (filesystem term) (currentDirectory term ++ path ++ "/")
  where
    path = splitBy ' ' (command step) !! 1

backOneDir :: String -> String
backOneDir path = foldl (\acc str -> acc ++ "/" ++ str) "" (drop 1 $ init $init (splitBy '/' path)) ++ "/"

applyLs :: Terminal -> Step -> Terminal
applyLs term (Step _ out) = foldl applyLsLine term out

applyLsLine :: Terminal -> String -> Terminal
applyLsLine term line
  | head splitLine == "dir" = newDir term (last splitLine)
  | otherwise = newFile term splitLine
  where
    splitLine = splitBy ' ' line

newDir :: Terminal -> String -> Terminal
newDir term name =
  Terminal
    ( insert
        (dirname (getPwd term))
        (addChildDir (getPwd term) fqn)
        (insert fqn (Directory (currentDirectory term) [] [] (fqn)) (filesystem term))
    )
    (currentDirectory term)
  where
    fqn = (dirname $ getPwd term) ++ name ++ "/"

addChildDir :: Directory -> String -> Directory
addChildDir dir childDir = Directory (dirname dir) (childDir : subDirs dir) (files dir) (dirname dir)

newFile :: Terminal -> [String] -> Terminal
newFile term [] = term
newFile term [[]] = term
newFile term [_ : _] = term
newFile term (sz : name : _) = Terminal newFs (currentDirectory term)
  where
    newFs = insert (currentDirectory term) (addFile (getPwd term) (File (read sz) name)) (filesystem term)

addFile :: Directory -> File -> Directory
addFile (Directory p s f d) file = Directory p s (file : f) d

type Filesystem = Map String Directory

data Terminal = Terminal {filesystem :: Filesystem, currentDirectory :: String} deriving (Eq, Show)

getPwd :: Terminal -> Directory
getPwd term = fromMaybe root (lookup (currentDirectory term) (filesystem term))

getDirSizes :: Terminal -> [(String, Int)]
getDirSizes term = map (\dirName -> (dirName, getSizeOfDir (filesystem term) dirName)) (keys $ filesystem term)

getSizeOfDir :: Filesystem -> String -> Int
getSizeOfDir fs dirName
  | null (subDirs dir) = sum (map size (files dir))
  | otherwise = sum (map size (files dir)) + sum (map (getSizeOfDir fs) (subDirs dir))
  where
    dir = fromMaybe root (lookup dirName fs)

getKeysLessThanEqual :: Int -> [(a, Int)] -> [Int]
getKeysLessThanEqual max tuples = filter (<= max) (map snd tuples)

getKeysGreaterThanEqual :: Int -> [(a, Int)] -> [Int]
getKeysGreaterThanEqual min tuples = filter (>= min) (map snd tuples)

getUsedSpace :: Terminal -> Int
getUsedSpace term = totalDiskSpace - getSizeOfDir (filesystem term) "/"

getNeededSpace :: Terminal -> Int
getNeededSpace term = updateSpaceRequired - getUsedSpace term

sizeOfDirectoryToDeleteToMakeUpdateSpace :: Terminal -> Int
sizeOfDirectoryToDeleteToMakeUpdateSpace term = minimum (getKeysGreaterThanEqual (getNeededSpace term) (getDirSizes term))