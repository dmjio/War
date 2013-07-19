module Main where

import           Control.Concurrent  (threadDelay)
import           Control.Monad       (forM_, replicateM)
import           Control.Monad.RWS   (RWST, ask, evalRWST, get, modify, put,
                                      tell)
import           Control.Monad.State (State, evalState, liftIO, state)
import           Data.List           (sort)
import           System.Environment  (getArgs)
import           System.Random       (Random, StdGen, getStdGen, random, split)

-- | TypeDefs
type RanState = State StdGen
type Deck     = [Card]
type Name     = String
type Players  = (Name, Name)
type War      = RWST Players [String] (Deck, Deck, StdGen) IO ()

-- | Contructors
data Suit     = Clubs | Diamonds | Hearts | Spades deriving (Eq,Ord,Enum)
data Card     = Card Int Suit                      deriving (Ord)

-- | Card Show Instance
instance Show Card where
    show (Card val suit) = cardval val ++ cardsuit suit where
          cardval n | n > 1 && n < 11 = show n
                    | n == 11 = "J"
                    | n == 12 = "Q"
                    | n == 13 = "K"
                    | n == 14 = "A"
          cardsuit s = case s of
                         Clubs    -> "♣"
                         Diamonds -> "♦"
                         Hearts   -> "♥"
                         Spades   -> "♠"

instance Eq Card where (Card int1 _) == (Card int2 _) = int1 == int2

-- | Convenience Functions
logHand cardA cardB
    | cardA > cardB = tell ["p1"]
    | cardA < cardB = tell ["p2"]
    | otherwise     = tell ["war"]
prnt x = liftIO $ print x
pct p total = show ((*100) $ fromIntegral p / fromIntegral total) ++ "%"
delay = liftIO $ threadDelay 1000000

-- | Shuffler
shuffle :: (Ord a) => [a] -> RanState [a]
shuffle as = do
    let n = length as
    rs1 <- replicateM n (state random :: RanState Int)
    return . map snd . sort $ zip rs1 as

-- | Game Summarizer
gameSummary :: [String] -> String -> String -> IO ()
gameSummary stats p1Name p2Name = do
  putStrLn "------------STATS-------------"
  let p1 = length $ filter (=="p1") stats
      p2 = length $ filter (=="p2") stats
      wars = length $ filter (=="war") stats
      total = length stats
  putStrLn $ show total ++ " Total Games Played"
  putStrLn $ show wars ++ " Total Wars"
  putStrLn $ p1Name ++ " won " ++ show p1 ++ " times"
  putStrLn $ p2Name ++ " won " ++ show p2 ++ " times"
  putStrLn $ p1Name ++ " Winning %: " ++ pct p1 total
  putStrLn $ p2Name ++ " Winning %: " ++ pct p2 total

-- | Env Arg Parser
parseArgs :: IO (String, String)
parseArgs = do
  args <- getArgs
  return $ case args of
    []      -> ("Player One", "Player Two")
    [x]     -> (x, "Player Two")
    (x:y:_) -> (x, y)

main :: IO ()
main = do
  putStrLn "Welcome to War!"
  (p1,p2) <- parseArgs
  gen <- getStdGen
  let deck = do val <- [2..14]; suit <- [Clubs .. Spades]; return (Card val suit)
  (deck1, deck2, g2) <- reShuffle (deck, deck, gen)
  (_,stats) <- evalRWST gameLoop (p1,p2) (deck1, deck2, g2)
  gameSummary stats p1 p2

-- | reShuffle
reShuffle :: Monad m => (Deck, Deck, StdGen) -> m (Deck, Deck, StdGen)
reShuffle (deck1, deck2, gen) = do
  let (g1,g2) = split gen
      newDeck1 = evalState (shuffle deck1) g1
      newDeck2 = evalState (shuffle deck2) g2
  return (newDeck1, newDeck2, g2)

-- | Game Init
gameLoop :: War
gameLoop = do
  (p1:xs, p2:ys, gen) <- get
  (name1,name2) <- ask
  logHand p1 p2
  prnt $ show p1 ++ "  vs. " ++ show p2
  case compare p1 p2 of
    LT -> do prnt $ name1 ++ " Wins Round!"
             put (xs ++ [p1,p2], ys, gen)
    GT -> do prnt $ name2 ++ " Wins Round!"
             put (xs, ys ++ [p1,p2], gen)
    EQ -> do prnt "War"
             logHand p1 p2
             war 4
  evalWinners

-- | When war occurs
war :: Int -> War
war num = do
  (left, right, gen) <- get
  (name1,name2) <- ask
  let [(l,ls), (r,rs)] = map (splitAt num) [left,right]
  prnt "Player 1"
  forM_ l $ \x -> delay >> prnt x
  prnt "Player 2"
  forM_ r $ \x -> delay >> prnt x
  case compare (length l) (length r) of
    LT -> do prnt $ name2 ++ " Wins Round!"
             put (ls, right ++ left, gen)
    GT -> do prnt $ name1 ++ " Wins Round!"
             put (left ++ right, rs, gen)
    EQ -> case compare (last l) (last r) of
            LT -> do prnt $ name2 ++ " Wins Round!"
                     delay
                     logHand (last l) (last r)
                     put (ls, right ++ l, gen)
            GT -> do prnt $ name1 ++ " Wins Round!"
                     delay
                     logHand (last l) (last r)
                     put (left ++ r, rs, gen)
            EQ -> war (num + 4)

-- | Evaluate Winners
evalWinners :: War
evalWinners = do
  printScore
  (p1, p2) <- ask
  all@(deck1, deck2, g) <- get
  case (deck1, deck2) of
    (x, []) -> prnt $ p1 ++ " Wins!"
    ([], x) -> prnt $ p2 ++ " Wins!"
    otherwise -> modify (reShuffle all) >> gameLoop

-- | Print Score
printScore :: War
printScore = do
  (deck1, deck2, _) <- get
  prnt $ "SCORE: " ++ show (length deck1) ++ " - " ++ show (length deck2)

