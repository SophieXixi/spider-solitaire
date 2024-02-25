module Action where

--- BEGIN Imports
import System.Random.MWC (createSystemRandom)
import Data.Random (runRVar)
import Data.Random.List (shuffle)
import Definition
import Debug.Trace
import Data.Maybe
--- END Imports
--- Display helpers

displayPiles :: [Card] -> [Char]
displayPiles [] = ['\n']
displayPiles ((num, visible):t)
   | visible = show num ++ " " ++ displayPiles t
   | otherwise = "* " ++ displayPiles t


displayState :: InternalState -> IO ()
displayState state = do
  putStrLn ("piles completed: " ++ show (finishedSets state))
  putStrLn ("cards in reserve: 10 * " ++ show (length (remaining state)))
  putStrLn ("card selected: " ++ show (position state))
  putStr ("piles: \n" ++ concatMap displayPiles (piles state))

display :: State -> IO ()
display game = do
  displayState (gameState game)
  putStrLn ("available actions: \n" ++ show (actions game))

--- Generic helpers

foldrWithIndex :: ((a, Int) -> b -> b) -> b -> [a] -> b
foldrWithIndex fn acc lst = foldr fn acc (zip lst [0..(length lst - 1)])

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

revealFirstCard :: [Card] -> [Card]
revealFirstCard [] = []
revealFirstCard ((value,_):t) = (value,True):t

hasFullSuit :: [Card] -> Bool
hasFullSuit cards
  | length cards < 13 = False
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
  let checkForFullSuit pile (np, ns) = if hasFullSuit pile then ((revealFirstCard (drop 13 pile)):np, ns+1) else (pile:np, ns)
      (newPiles, addedSets) = foldr checkForFullSuit ([], 0) (piles state)
  in InternalState {
    piles = newPiles,
    remaining = remaining state,
    finishedSets = finishedSets state + addedSets,
    position = position state
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

-- Move the cards at or below position to the target pile. Also reveals the card above the position if it is not revealed
-- first Position is from position, second Pile is target pile
moveCards :: InternalState -> Position -> Pile -> InternalState
moveCards state (fromPile, fromIndex) toPile =
  let oldPiles = piles state
      removeCardsFromPile (cards, i) (cp, ctm) =
        if i == fromPile
          then (let (bf, aft) = splitAt (fromIndex + 1) cards in ((revealFirstCard aft):cp, bf))
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


-- when user click dealCards:
--      takes the first set of cards
--      put each card to the top of each pile
-- by data structure:
--      take the first list in the list of remaining cards
--      put each element of this list to the end of the list
dealCards :: InternalState -> InternalState
dealCards state = 
    case remaining state of
        [] -> state
        (firstset:restset) ->
            InternalState
                {
                    remaining = restset,
                    piles = distributedCards firstset (piles state),
                    finishedSets = finishedSets state,
                    position = position state
                }
    
distributedCards:: [Card] -> [[Card]] -> [[Card]]
distributedCards [] _ = []
distributedCards _ [] = []
distributedCards ((num, visible):t) (hp:tp) = ((num, True):hp) : distributedCards t tp

-- given the state of the game, and the target position of the card, return true if the card can be moved, and false otherwise
-- assume the position is valid
-- the card can be moved if: 
--      it is the first one of one pile from bottom up (the first elem in the list)
--      all the cards before it (in the list) are continuous
canChoose :: InternalState -> Position -> Bool
canChoose game (column, row)
    | row == 0 = True
    | otherwise = trace ("display: " ++ show list) comparePile list 0 row 
    where 
        list = getColumn (piles game) column

getColumn:: [[Card]] -> Int -> [Card]
getColumn [] _ = []
getColumn (h:t) column
--    | column == 0 = trace ("display: " ++ show h) h
    | column == 0 = h
--    | otherwise = displayPiles (getColumn t (column-1))
    | otherwise = getColumn t (column-1)

comparePile:: [Card] -> Int -> Int -> Bool
comparePile [] _ _ = trace ("comparePile0") False
comparePile ((num, visible):t) pre 0
    | not visible = False
    | num == (pre+1) = True
    | otherwise = False
-- comparePile [(num, visible)] _ 0 = trace ("comparePile1") True
comparePile ((num, visible):t) pre pos
    | not visible  = trace ("visible: " ++ show visible) False
    | pre == 0 = trace ("pre=0") comparePile t num (pos-1)
--    | pos == 0 = trace ("pos=0") True
    | num == (pre+1) = trace ("num+1") comparePile t num (pos-1)
    | otherwise = trace ("other") False


-- given an internal state, and an action, return a bool to tell whether the action can be performed:
-- if the action is:
--      choose: the internal state has no position stored => if the card can be chosen => save to the internal state
--              the internal state has a position stored => return false, cannot be performed
--      deal: if the remaining has cards left => return true
--            if the remaining is empty, no cards left to be dealed => return false
--      move: if the internal state has a position stored => if the cards can be moved (the first elem in the pile = the num of the chosen position+1)
--            if the internal state has no position stored => return false
canActionBePerformed :: InternalState -> Action -> Bool
canActionBePerformed game (Choose (column, row))
    | isJust (position game) = False
    | canChoose game (column,row) = True
    | otherwise = False
canActionBePerformed game (Move pile)
    | not (isJust (position game)) = False
    | null (getColumn (piles game) pile)  = True
    | dest == chosen + 1 = True
    | otherwise = False
    where 
        chosen = getCardNum game (fromJust (position game))
        dest = getCardNum game (pile, 0)

canActionBePerformed game (Deal)
    | (remaining game) == [] = False
    | otherwise = True

-- update the internal state, showing one card has been chosen
chooseCard :: InternalState -> Position -> InternalState
chooseCard game pos = game { position = Just pos }

-- get the number of the card given the position
getCardNum :: InternalState -> Position -> Int
-- getCardNum _ Nothing = 0
getCardNum game (col, row) = getCard (getColumn (piles game) col) row

-- get the number of a card
getCard :: [Card] -> Int -> Int
getCard [] _ = 0
getCard ((num, visible):t) pos
    | pos == 0 = num
    | otherwise = getCard t (pos-1)