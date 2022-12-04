import Util

main :: IO ()
main = getSolutions "P02" part1 part2 >>= \solution -> putStrLn solution

data Game = Game {opponentMove :: Hand, myMove :: Hand} deriving (Eq, Show)

data Hand = ROCK | PAPER | SCISSORS deriving (Eq, Show, Enum)

data Result = WIN | TIE | LOSS deriving (Eq, Show, Enum)

part1 :: String -> String
part1 input = show $ sum $ map (getPointsForGame . parseGame) (getNonEmptyLines input)

parseGame :: String -> Game
parseGame (opp : _ : mine : _) =
  Game (parseHand opp) (parseHand mine)

parseHand :: Char -> Hand
parseHand char
  | char == 'A' = ROCK
  | char == 'B' = PAPER
  | char == 'C' = SCISSORS
  | char == 'X' = ROCK
  | char == 'Y' = PAPER
  | char == 'Z' = SCISSORS
  | otherwise = ROCK

getPointsForGame :: Game -> Int
getPointsForGame game = (getPointsForResult (getResultForGame game)) + (getPointsForHand (myMove game))

getResultForGame :: Game -> Result
getResultForGame (Game opp my)
  | winningHand opp == my = WIN
  | losingHand opp == my = LOSS
  | otherwise = TIE

getPointsForResult :: Result -> Int
getPointsForResult WIN = 6
getPointsForResult TIE = 3
getPointsForResult LOSS = 0

getPointsForHand :: Hand -> Int
getPointsForHand ROCK = 1
getPointsForHand PAPER = 2
getPointsForHand SCISSORS = 3

winningHand :: Hand -> Hand
winningHand ROCK = PAPER
winningHand PAPER = SCISSORS
winningHand SCISSORS = ROCK

losingHand :: Hand -> Hand
losingHand ROCK = SCISSORS
losingHand PAPER = ROCK
losingHand SCISSORS = PAPER

part2 :: String -> String
part2 input = show $ sum $ map (getPointsForGame . gameFromRequiredResult . parseStrategy) (getNonEmptyLines input)

data Strategy = Strategy {opposingHand :: Hand, result :: Result} deriving (Eq, Show)

parseStrategy :: String -> Strategy
parseStrategy (opp : _ : res : _) = Strategy (parseHand opp) (parseResult res)

getOpponentHandChar :: String -> Char
getOpponentHandChar = head

parseResult :: Char -> Result
parseResult char
  | char == 'X' = LOSS
  | char == 'Y' = TIE
  | char == 'Z' = WIN
  | otherwise = LOSS

requiredHand :: Strategy -> Hand
requiredHand strategy
  | result strategy == WIN = winningHand (opposingHand strategy)
  | result strategy == LOSS = losingHand (opposingHand strategy)
  | otherwise = (opposingHand strategy)

gameFromRequiredResult :: Strategy -> Game
gameFromRequiredResult strategy = Game (opposingHand strategy) (requiredHand strategy)
