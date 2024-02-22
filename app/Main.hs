module Main where

--- BEGIN Imports
import System.Random.MWC (createSystemRandom)
import Data.Random (runRVar)
import Data.Random.List (shuffle)
--- END Imports

data State = State {
  gameState :: InternalState,  -- internal_state
  actions :: [Action]          -- available_actions
}

data Result = EndOfGame Double State    -- end of game: value, starting state
            | ContinueGame State        -- continue with new state
        --  deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> Action

-- Spider

type Card = (Int, Bool)

-- data Card = Card Int Bool

type Pile = Int

type Position = (Pile, Int) -- (which pile, which index in pile)

-- arguments: 10 piles, reserve, number of foundation piles completed, current selected card position
-- data InternalState = InternalState [[Card]] [[Card]] Int (Maybe Position)
-- remaining: the first set to be distributed is the first element
-- piles of cards: the left is bottom, and right is up
-- finishedSets: number of foundation piles completed
data InternalState = InternalState {
  remaining :: [[Card]],
  piles :: [[Card]],
  finishedSets :: Int,
  position :: Maybe Position
} deriving (Show)

data Action = Choose Position
            | Move Pile
            | Deal
  deriving Show

--- Display helpers

displayPiles :: [Card] -> [Char]
displayPiles [] = ['\n']
displayPiles ((num, visible):t)
   | visible = show num ++ " " ++ displayPiles t
   | otherwise = "* " ++ displayPiles t


displayState :: InternalState -> IO ()
displayState state = do
  putStrLn ("piles completed: " ++ (show (finishedSets state)))
  putStrLn ("cards in reserve: 10 * " ++ (show (length (remaining state))))
  putStrLn ("card selected: " ++ (show (position state)))
  putStr ("piles: \n" ++ (concatMap displayPiles (piles state)))

display :: State -> IO ()
display game = do
  displayState (gameState game)
  putStrLn ("available actions: \n" ++ (show (actions game)))

--- Generic helpers

foldrWithIndex :: ((a, Int) -> b -> b) -> b -> [a] -> b
foldrWithIndex fn acc lst = foldr fn acc (zip lst [0..((length lst) - 1)])

--- Helper methods

-- Returns a randomly shuffled version of the given list
shuffleList :: [a] -> IO [a]
shuffleList lst = do
  gen <- createSystemRandom
  shuffledList <- runRVar (shuffle lst) gen
  return shuffledList

-- Contains all 104 card values (8 sets of cards 1 to 13)
allCardValues :: [Int]
allCardValues = concat [replicate 8 i | i <- [1..13]]

-- Splits 54 card values into 10 piles of card values
splitTableauValues :: [Int] -> [[Int]]
splitTableauValues lst = fst (foldr accum ([], lst) [6,6,6,6,5,5,5,5,5,5])
  where
    accum e (acc, lt) =
      let (h, r) = splitAt e lt in
        (h:acc, r)

-- Splits 50 card values into 5 sets of 10 cards for the reserve
splitReserveValues :: [Int] -> [[Int]]
splitReserveValues lst = fst (foldr accum ([], lst) (replicate 5 10))
  where
    accum e (acc, lt) =
      let (h, r) = splitAt e lt in
        (h:acc, r)

-- Turns Int values into Cards where only the first card in the pile is face up
tableauValuesToCards :: [[Int]] -> [[Card]]
tableauValuesToCards values = [foldl accum [] pileValues | pileValues <- values]
  where
    accum (h:t) e = (h:t)++[(e, False)]
    accum [] e = [(e, True)]

generateInitialState :: IO State
generateInitialState = do
  shuffledCardValues <- shuffleList allCardValues
  let (tableauValues, reserveValues) = splitAt 54 shuffledCardValues
  let tableauPileValues = splitTableauValues tableauValues
  let tableauCards = tableauValuesToCards tableauPileValues
  let reserveCards = [map (\ val -> (val, True)) values | values <- (splitReserveValues reserveValues)]
  return (State {
    gameState = InternalState {
      piles = tableauCards,
      remaining = reserveCards,
      finishedSets = 0,
      position = Nothing
    },
    actions = []
  })

hasFullSuit :: [Card] -> Bool
hasFullSuit cards
  | (length cards) < 13 = False
  | otherwise = foldrWithIndex (\ ((value, faceUp), i) acc -> acc && faceUp && value == (i + 1)) True (take 12 cards)
{- TESTS
  putStrLn (show (hasFullSuit [(1, True), (2, True), (3, True), (4, True), (5, True), (6, True), (7, True), (8, True), (9, True), (10, True), (11, True), (12, True), (13, True)]))
  putStrLn (show (hasFullSuit [(1, True), (2, True), (3, True), (4, True), (5, True), (6, True), (7, True), (8, True), (9, True), (10, True), (11, True), (12, True), (13, True), (3, False)]))
  putStrLn (show (hasFullSuit [(1, True), (2, True)]))
  putStrLn (show (hasFullSuit [(1, True), (2, True), (3, True), (4, True), (5, True), (6, True), (7, True), (8, True), (9, True), (10, True), (11, False), (12, True), (13, True)]))
  putStrLn (show (hasFullSuit [(1, True), (2, True), (3, True), (4, True), (5, True), (6, True), (8, True), (9, True), (10, True), (11, True), (12, True), (13, True)]))
-}

tryToCompleteFoundationPile :: InternalState -> InternalState
tryToCompleteFoundationPile state =
  let checkForFullSuit pile (np, ns) = if hasFullSuit pile then ((drop 13 pile):np, ns+1) else (pile:np, ns)
      (newPiles, addedSets) = (foldr checkForFullSuit ([], 0) (piles state))
  in InternalState {
    piles = newPiles,
    remaining = (remaining state),
    finishedSets = (finishedSets state) + addedSets,
    position = (position state)
  }
{- TEST
  let state = InternalState {
    piles = [[(1, True), (2, True)], [(1, True), (2, True), (3, True), (4, True), (5, True), (6, True), (7, True), (8, True), (9, True), (10, True), (11, True), (12, True), (13, True), (2, False)]],
    remaining = [],
    finishedSets = 0,
    position = Nothing
  }
  displayState state
  displayState (tryToCompleteFoundationPile state)
-}

-- first Position is from position, second Pile is target pile
moveCards :: InternalState -> Position -> Pile -> InternalState
moveCards state (fromPile, fromIndex) toPile =
  let oldPiles = piles state
      removeCardsFromPile (cards, i) (cp, ctm) =
        if i == fromPile
          then (let (bf, aft) = splitAt (fromIndex + 1) cards in (aft:cp, bf))
          else (cards:cp, ctm)
      (curPiles, cardsToMove) = foldrWithIndex removeCardsFromPile ([], []) oldPiles
      addCardsToPile (cards, i) np =
        if i == toPile
          then (cardsToMove++cards):np
          else cards:np
      newPiles = foldrWithIndex addCardsToPile [] curPiles
  in InternalState {
    piles = newPiles,
    remaining = remaining state,
    finishedSets = finishedSets state,
    position = position state
  }
{- TEST
  let state = InternalState {
    piles = [[(1, True), (2, True), (3, False), (4, False)], [(1, True), (2, True), (3, True), (4, True), (5, True), (6, True), (7, True), (8, True), (9, True), (10, True), (11, True), (12, True), (13, True), (2, False)]],
    remaining = [],
    finishedSets = 0,
    position = Nothing
  }
  displayState state
  displayState (moveCards state (0, 1) 1)
-}

-- -- 3 methods above to be completed by William
-- -- 3 methods below to be completed by Sophie


-- dealCards :: InternalState -> InternalState

-- canChoose :: State -> Position -> Bool

-- getAvailableActions :: Action -> InternalState -> [Action]





main :: IO ()
main = do
  res <- generateInitialState
  display res
