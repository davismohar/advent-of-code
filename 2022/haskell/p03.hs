import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace
import Util

main :: IO ()
main = getSolutions "P03" part1 part2 >>= \solution -> putStrLn solution

data Rucksack = Rucksack {firstCompartment :: String, secondCompartment :: String} deriving (Eq, Show)

type Group = (Rucksack, Rucksack, Rucksack)

part1 :: String -> String
-- part1 input = show $ sum $ map (toPriority . getDuplicate . parseRucksack) (getLines input)
part1 input = show $ sum $ map (toPriority . getDuplicate . parseRucksack) (getLines input)

parseRucksack :: String -> Rucksack
parseRucksack input = uncurry Rucksack (splitAt ((length input + 1) `div` 2) input)

getDuplicate :: Rucksack -> Char
getDuplicate rucksack = head $ getCommonLetters (firstCompartment rucksack) (secondCompartment rucksack)

getCommonLetters :: String -> String -> String
getCommonLetters [] _ = []
getCommonLetters [a] str2 = hasChar a str2
getCommonLetters str1 str2 = hasChar (head str1) str2 ++ getCommonLetters (tail str1) str2

hasChar :: Char -> String -> String
hasChar char string = maybe "" (: []) (maybeHasChar char string)

maybeHasChar :: Char -> String -> Maybe Char
maybeHasChar char = find (== char)

startOfLowercaseLetters = 97

endOfLowercaseLetters = startOfLowercaseLetters + 25

startOfUppercaseLetters = 65

toPriority :: Char -> Int
toPriority char
  | asciiValue >= startOfLowercaseLetters && asciiValue <= endOfLowercaseLetters = asciiValue - startOfLowercaseLetters + 1
  | otherwise = asciiValue - startOfUppercaseLetters + 26 + 1
  where
    asciiValue = toAsciiValue char

toAsciiValue :: Char -> Int
toAsciiValue = ord

part2 :: String -> String
part2 input = show $ sum $ map (toPriority . getBadge) $ getGroups $ map parseRucksack $ getLines input

getGroups :: [Rucksack] -> [Group]
getGroups [] = []
getGroups (a : b : c : d) = (a, b, c) : getGroups d

fstTriple (a, _, _) = a

sndTriple (_, b, _) = b

thrdTriple (_, _, c) = c

getBadge :: Group -> Char
getBadge group = head $ getCommonLetters (getCommonLetters (allItems $ fstTriple group) (allItems $ sndTriple group)) (allItems $ thrdTriple group)

allItems :: Rucksack -> String
allItems rucksack = firstCompartment rucksack ++ secondCompartment rucksack
