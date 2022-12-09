import Control.Arrow (Arrow (first))
import Control.Monad.RWS (All (getAll))
import Util

main :: IO ()
main = getSolutions "P06" part1 part2 >>= putStrLn

part1 :: String -> String
part1 input = show $ map (findIndexOfFirstMarker . getAllNCharSubstrings 4) $ getLines input

part2 :: String -> String
part2 input = show $ map (findIndexOfFirstMarker . getAllNCharSubstrings 14) $ getLines input

findIndexOfFirstMarker :: [(String, Int)] -> Int
findIndexOfFirstMarker chunks = snd firstChunk + length (fst firstChunk)
  where
    firstChunk = head (filter (not . hasDuplicateChars . fst) chunks)

getAllNCharSubstrings :: Int -> String -> [(String, Int)]
getAllNCharSubstrings n string = getAllNCharSubstringsOfStringWithLength 0 n (length string) string

getAllNCharSubstringsOfStringWithLength :: Int -> Int -> Int -> String -> [(String, Int)]
getAllNCharSubstringsOfStringWithLength index n len string
  | index + n >= len = [(take n string, index)]
  | otherwise = (take n string, index) : getAllNCharSubstringsOfStringWithLength (index + 1) n len (tail string)

hasDuplicateChars :: String -> Bool
hasDuplicateChars [] = False
hasDuplicateChars [_] = False
hasDuplicateChars (a : rst)
  | a `elem` rst = True
  | otherwise = hasDuplicateChars rst
