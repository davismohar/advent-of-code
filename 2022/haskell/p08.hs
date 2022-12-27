import Data.List (transpose)
import Data.Sequence (mapWithIndex)
import Util

data Tree = Tree {visible :: Bool, height :: Int} deriving (Eq, Show)

main :: IO ()
main = getSolutions "P08" part1 part2 >>= putStrLn

part1 :: String -> String
part1 input = show $getVisibleTreeCount $ reduce $ normalizeRotations $ map (toVisibilityMap . toTreeMap) $ getAllPerspectives $ parseTreeHeights $ getLines input

-- this is really really awful - I misunderstood the question initially where I thought a tree had to be not visible to be eligible. Hate this question too much to fix.
part2 :: String -> String
part2 input =
  let heightMap = parseTreeHeights $ getLines input
   in show $ maximum $ map (product . getVisibleTreesFromCoordinate heightMap) (getCoords $reduce $ normalizeRotations $ map (toVisibilityMap . toTreeMap) $ getAllPerspectives $ heightMap)

parseTreeHeights :: [String] -> [[Int]]
parseTreeHeights = map (map (\char -> read [char]))

toTreeMap :: [[Int]] -> [[Tree]]
toTreeMap = map toTrees

toVisibilityMap :: [[Tree]] -> [[Bool]]
toVisibilityMap = map (map visible)

toTrees :: [Int] -> [Tree]
toTrees heights =
  foldr
    addTreeOfHeight
    [Tree True (last heights)]
    (init heights)

addTreeOfHeight :: Int -> [Tree] -> [Tree]
addTreeOfHeight h trees = Tree (h > maximum (map height trees)) h : trees

rotl :: [[x]] -> [[x]]
rotl = transpose . map reverse

left :: [[x]] -> [[x]]
left arr = rotl (rotl (rotl arr))

right :: [[x]] -> [[x]]
right arr = rotl arr

oneEighty :: [[x]] -> [[x]]
oneEighty arr = (rotl (rotl arr))

getAllPerspectives :: [[Int]] -> [[[Int]]]
getAllPerspectives treeHeights = treeHeights : left treeHeights : oneEighty treeHeights : [right treeHeights]

normalizeRotations :: [[[Bool]]] -> [[[Bool]]]
normalizeRotations a = head a : right (a !! 1) : oneEighty (a !! 2) : [left (a !! 3)]

reduce :: [[[Bool]]] -> [[Bool]]
reduce [[]] = [[]]
reduce [a] = a
reduce arrays = zipWith (zipWith (||)) (head arrays) (reduce (tail arrays))

getVisibleTreeCount :: [[Bool]] -> Int
getVisibleTreeCount trees = sum (map trueCount trees)

trueCount :: [Bool] -> Int
trueCount bools = length (filter (== True) bools)

tagCoords :: [[Bool]] -> [[(Bool, (Int, Int))]]
tagCoords = zipWith tagWithYCoord [0 ..]

tagWithYCoord :: Int -> [Bool] -> [(Bool, (Int, Int))]
tagWithYCoord y = zipWith (\x elem -> (elem, (x, y))) [0 ..]

getCoords :: [[Bool]] -> [(Int, Int)]
getCoords arr = map snd (concat (tagCoords arr))

getVisibleTreesFromCoordinate :: [[Int]] -> (Int, Int) -> [Int]
getVisibleTreesFromCoordinate grid coords =
  (getVisibleTreesAbove (coords) grid) :
  (getVisibleTreesBelow coords grid) :
  (getVisibleTreesLeft coords grid) :
  [(getVisibleTreesRight coords grid)]

getVisibleTreesAbove coords@(x, y) grid = lengthOfSightline (grid !! y !! x) (getTreesAbove (coords) (grid))

getVisibleTreesRight coords@(x, y) grid = lengthOfSightline (grid !! y !! x) (getTreesRight (coords) (grid))

getVisibleTreesBelow coords@(x, y) grid = lengthOfSightline (grid !! y !! x) (getTreesBelow coords grid)

getVisibleTreesLeft coords@(x, y) grid = lengthOfSightline (grid !! y !! x) (getTreesLeft coords grid)

getTreesAbove (x, y) grid = map (\i -> grid !! i !! x) (reverse [0 .. y - 1])

getTreesLeft (x, y) grid = map (\i -> grid !! y !! i) (reverse [0 .. x - 1])

getTreesBelow (x, y) grid = map (\i -> grid !! i !! x) [y + 1 .. (length grid) - 1]

getTreesRight (x, y) grid = map (\i -> grid !! y !! i) [x + 1 .. length (head grid) - 1]

lengthOfSightline :: Int -> [Int] -> Int
lengthOfSightline height trees
  | null trees = 0
  | height > head trees = 1 + lengthOfSightline height (tail trees)
  | otherwise = 1
