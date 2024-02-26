module Action where

--- BEGIN Imports
import System.Random.MWC (createSystemRandom)
import Data.Random (runRVar)
import Data.Random.List (shuffle)
import Definition
import Debug.Trace
import Data.Maybe
import Data.List
import qualified Data.HashSet as HashSet
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
  putStr ("piles: \n" ++ concatMap (\ (i,pile) -> "[" ++ show i ++ "] " ++ displayPiles pile) (zip [0..] (piles state)))

display :: State -> IO ()
display game = do
  displayState (gameState game)
  putStrLn ("available actions: \n" ++ show (actions game))

instance Show InternalState where
  show state =
    "===================================\n" ++ concatMap (\ (i,pile) -> "[" ++ show i ++ "] " ++ displayPiles pile) (zip [0..] (piles state)) ++
    "remaining: " ++ show (length (remaining state)) ++ ", completed: " ++ show (finishedSets state) ++
    "\n===================================\n"

showComplete :: InternalState -> String
showComplete state = "InternalState {\n  piles = " ++ show (piles state) ++ ",\n  remaining = " ++ show (remaining state) ++ ",  \n  finishedSets = " ++ show (finishedSets state) ++ ",\n  position = " ++ show (position state) ++ "\n}"

showCondensed :: InternalState -> String
showCondensed state = concatMap displayPiles (piles state) ++ show (length (remaining state)) ++ "," ++ show (finishedSets state)

showSolution :: Maybe [Maybe CardMove] -> String
showSolution Nothing = "No solution"
showSolution (Just lst) =
  concatMap showMove lst where
    showMove Nothing = "\nDeal"
    showMove (Just move) = "\n" ++ show move

--- Generic helpers

foldrWithIndex :: ((a, Int) -> b -> b) -> b -> [a] -> b
foldrWithIndex fn acc lst = foldr fn acc (zip lst [0..(length lst - 1)])

-- findIndices :: (a -> Bool) -> [a] -> [Int]
-- findIndices fn = foldrWithIndex (\ (e,i) acc -> (if fn e then i:acc else acc)) []

appendIfExists :: a -> Maybe [a] -> Maybe [a]
appendIfExists val Nothing = Nothing
appendIfExists val (Just lst) = Just (val:lst)

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

-- Returns a random initial game state, where cards are shuffled before putting in the piles and remaining fields
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

-- Make first card of a list of cards face up
revealFirstCard :: [Card] -> [Card]
revealFirstCard [] = []
revealFirstCard ((value,_):t) = (value,True):t

-- Returns true iff the first 13 cards of the list make up a full suit
hasFullSuit :: [Card] -> Bool
hasFullSuit cards
  | length cards < 13 = False
  | otherwise = foldrWithIndex (\ ((value, faceUp), i) acc -> acc && faceUp && value == (i + 1)) True (take 13 cards)
{- TESTS
  putStrLn (show (hasFullSuit [(1, True), (2, True), (3, True), (4, True), (5, True), (6, True), (7, True), (8, True), (9, True), (10, True), (11, True), (12, True), (13, True)]))
  putStrLn (show (hasFullSuit [(1, True), (2, True), (3, True), (4, True), (5, True), (6, True), (7, True), (8, True), (9, True), (10, True), (11, True), (12, True), (13, True), (3, False)]))
  putStrLn (show (hasFullSuit [(1, True), (2, True)]))
  putStrLn (show (hasFullSuit [(1, True), (2, True), (3, True), (4, True), (5, True), (6, True), (7, True), (8, True), (9, True), (10, True), (11, False), (12, True), (13, True)]))
  putStrLn (show (hasFullSuit [(1, True), (2, True), (3, True), (4, True), (5, True), (6, True), (8, True), (9, True), (10, True), (11, True), (12, True), (13, True)]))
  putStrLn (show (hasFullSuit [(1, True), (2, True), (3, True), (4, True), (5, True), (6, True), (7, True), (8, True), (9, True), (10, True), (11, True), (12, True), (13, False)]))
-}

-- If full suit(s) exist in the piles, remove them and add to the completed foundation piles
-- When removing the full suit, the card above the last one will be revealed if it is hidden
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

-- Alternative function for moving cards that uses the CardMove data type
moveCardsAlt :: InternalState -> CardMove -> InternalState
moveCardsAlt state cardMove = moveCards state (from cardMove) (to cardMove)

-- Given the piles, return all possible card moves
getPossibleCardMoves :: [[Card]] -> [CardMove]
getPossibleCardMoves cardPiles =
  let cardCanBePut :: Int -> [Card] -> Bool
      cardCanBePut _ [] = True
      cardCanBePut v ((hvalue,_):_) = (v + 1) == hvalue
      getMovesForCard :: (Card,Position) -> [CardMove]
      getMovesForCard ((value,up),pos) =
        if up
          then map (\ target -> CardMove { from = pos, to = target }) (findIndices (cardCanBePut value) cardPiles)
          else []
      cardsWithPositions :: [Card] -> Pile -> [(Card,Position)]
      cardsWithPositions cards pileIndex = foldrWithIndex (\ (card,i) acc -> (card,(pileIndex,i)):acc) [] cards
      getMovesForPile :: [(Card,Position)] -> [CardMove]
      getMovesForPile (((hvalue,hup),hpos):((nvalue,nup),npos):r) =
        getMovesForCard ((hvalue,hup),hpos) ++ (if nup && ((hvalue + 1) == nvalue) then getMovesForPile (((nvalue,nup),npos):r) else [])
      getMovesForPile [h] = getMovesForCard h
      getMovesForPile [] = []
  in foldrWithIndex (\ (cards,i) acc -> (getMovesForPile (cardsWithPositions cards i))++acc) [] cardPiles
{- TEST
  let state = InternalState {
    piles = [[(4, True), (5, True), (6, False)], [(6, True), (8, True)], [(7, True)], [(4, True), (5, False)], []],
    remaining = [],
    finishedSets = 0,
    position = Nothing
  }
  displayState state
  putStrLn ("Possible Moves:" ++ show (getPossibleCardMoves (piles state)))
-}

-- Given a game state, returns an integer representing the value; the higher the better
getValueOfState :: InternalState -> Int
getValueOfState state =
  let getContinuousCards :: [Card] -> [Card]
      getContinuousCards [] = []
      getContinuousCards [(val,up)] = if up then [(val,up)] else []
      getContinuousCards ((val,up):(nval,nup):r)
        | not up = []
        | (val + 1) == nval = (val,up):(getContinuousCards ((nval,nup):r))
        | otherwise = [(val,up)]
      computePileValue :: [Card] -> Int
      computePileValue cards = if null cards then 1 else 2 * (length (getContinuousCards cards) - length (filter (\ (_,up) -> not up) cards))
  in (sum (map computePileValue (piles state))) + (30 * finishedSets state) -- completed sets has value larger than 26 to encourage completing sets

-- returns the state after the card move is performed
performCardMove :: InternalState -> CardMove -> InternalState
performCardMove state move = tryToCompleteFoundationPile (moveCardsAlt state move)

getPossibleCardMovesWithValue :: InternalState -> [(Int,CardMove)]
getPossibleCardMovesWithValue state = map (\ move -> (getValueOfState (performCardMove state move),move)) (getPossibleCardMoves (piles state))

-- Given a game state, returns the best next card move according to the value of the state after the move, or Nothing if there is no card moves available
suggestNextCardMove :: InternalState -> Maybe CardMove
suggestNextCardMove state =
  let cardMovesWithValue = getPossibleCardMovesWithValue state in
    if null cardMovesWithValue
      then Nothing
      else
        trace (show cardMovesWithValue) (Just (snd (foldr (\ (val,move) (maxval,maxmove) -> if val > maxval then (val,move) else (maxval,maxmove)) (head cardMovesWithValue) cardMovesWithValue)))
{-TEST
  let state = InternalState {
    piles = [[(4, True), (11, True), (12, True), (13, True), (5, False), (6, False)], [(5, True), (9, True), (11, False), (1, False), (3, False)], [(3, True), (9, True), (11, False)], [(13, True), (2, True), (5, False)], [(4, True), (4, True), (5, True), (6, True), (8, False), (9, False)], [(6, True), (6, True), (7, True), (8, True), (9, True), (10, False)], [(9, True), (4, True)], [(13, True), (1, True), (3, False)], [(12, True), (8, True)], [(10, True), (12, True)]],
    remaining = [],
    finishedSets = 0,
    position = Nothing
  }
  displayState state
  putStrLn ("Possible Moves:" ++ show (getPossibleCardMoves (piles state)))
  putStrLn ("Current Value:" ++ show (getValueOfState state))
  putStrLn ("Suggested Move:" ++ show (suggestNextCardMove state))
-}

-- given the current state, returns a solution to the game in the form of [CardMove, or Nothing if dealing from reserve], or nothing if a solution does not exist
solveGame :: InternalState -> Maybe [Maybe CardMove]
solveGame state =
  trace (show state) (if finishedSets state == 8 then trace "soln found" (Just []) else
    let currentValue = getValueOfState state
        cardMovesWithValue = filter (\ (val,_) -> val > currentValue) (getPossibleCardMovesWithValue state) in
      trace ("currentValue = " ++ (show currentValue) ++ "; moves = " ++ (show (cardMovesWithValue))) (if null cardMovesWithValue
        then
          if null (remaining state) then Nothing else (trace "deal cards" (appendIfExists Nothing (solveGame (dealCards state))))
        else
          let solveWithMove :: CardMove -> Maybe [Maybe CardMove]
              solveWithMove move = trace ("try move: " ++ (show move)) (appendIfExists (Just move) (solveGame (performCardMove state move)))
              getFirstSolution :: [(Int,CardMove)] -> Maybe [Maybe CardMove]
              getFirstSolution [] = Nothing
              getFirstSolution ((_,hmove):r) =
                let potentialSoln = solveWithMove hmove
                  in if isNothing potentialSoln then getFirstSolution r else potentialSoln
            in getFirstSolution (sortBy (\ (a,_) (b,_) -> compare a b) cardMovesWithValue)))

type Mem = HashSet.HashSet String

appendIfExistsMem :: a -> String -> (Maybe [a], Mem) -> (Maybe [a], Mem)
appendIfExistsMem val str (Nothing, visited) = (Nothing, HashSet.insert str visited)
appendIfExistsMem val _ (Just lst, visited) = (Just (val:lst), visited)

solveGameMemWithTrace :: InternalState -> Mem -> (Maybe [Maybe CardMove], Mem)
solveGameMemWithTrace state visited =
  if finishedSets state == 8 then trace "soln found" (Just [], visited) else
    let stateStr = showCondensed state
      in if HashSet.member stateStr visited then (Nothing, visited) else
        let currentValue = getValueOfState state
            cardMovesWithValue = filter (\ (val,_) -> val > currentValue) (getPossibleCardMovesWithValue state)
          in trace ("currentValue = " ++ show currentValue ++ "; moves = " ++ show (cardMovesWithValue) ++ "\n" ++ show state) (if null cardMovesWithValue
            then
              if null (remaining state) then (Nothing, HashSet.insert stateStr visited) else appendIfExistsMem Nothing stateStr (solveGameMem (dealCards state) visited)
            else
              let getFirstSolution :: [(Int,CardMove)] -> Mem -> (Maybe [Maybe CardMove], Mem)
                  getFirstSolution [] curVisited = (Nothing, curVisited)
                  getFirstSolution ((_,move):r) curVisited =
                    let (potentialSoln, newVisited) = solveGameMem (performCardMove state move) curVisited
                        soln = appendIfExists (Just move) potentialSoln
                      in if isNothing soln then getFirstSolution r newVisited else (soln, newVisited)
                  (firstSolution, nextVisited) = getFirstSolution (sortBy (\ (a,_) (b,_) -> compare a b) cardMovesWithValue) visited
                in if isNothing firstSolution then (Nothing, HashSet.insert stateStr nextVisited) else (firstSolution, nextVisited))

-- This is extremely efficient. I ran this with random initialization for about 100 times, and all of them ran below 1 second. There might be extremely rare instances where it can take up to 30 seconds, but that only occured before I removed all the trace outputs.
solveGameMem :: InternalState -> Mem -> (Maybe [Maybe CardMove], Mem)
solveGameMem state visited =
  if finishedSets state == 8 then (Just [], visited) else
    let stateStr = showCondensed state
      in if HashSet.member stateStr visited then (Nothing, visited) else
        let currentValue = getValueOfState state
            cardMovesWithValue = filter (\ (val,_) -> val > currentValue) (getPossibleCardMovesWithValue state)
          in if null cardMovesWithValue
            then
              if null (remaining state) then (Nothing, HashSet.insert stateStr visited) else appendIfExistsMem Nothing stateStr (solveGameMem (dealCards state) visited)
            else
              let getFirstSolution :: [(Int,CardMove)] -> Mem -> (Maybe [Maybe CardMove], Mem)
                  getFirstSolution [] curVisited = (Nothing, curVisited)
                  getFirstSolution ((_,move):r) curVisited =
                    let (potentialSoln, newVisited) = solveGameMem (performCardMove state move) curVisited
                        soln = appendIfExists (Just move) potentialSoln
                      in if isNothing soln then getFirstSolution r newVisited else (soln, newVisited)
                  (firstSolution, nextVisited) = getFirstSolution (sortBy (\ (a,_) (b,_) -> compare a b) cardMovesWithValue) visited
                in if isNothing firstSolution then (Nothing, HashSet.insert stateStr nextVisited) else (firstSolution, nextVisited)

solveGameMemRun :: InternalState -> Maybe [Maybe CardMove]
-- solveGameMemRun state = fst (solveGameMemWithTrace state HashSet.empty)
solveGameMemRun state = fst (solveGameMem state HashSet.empty)

-- -- 3 methods above to be completed by William
-- -- 3 methods below to be completed by Sophie

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


-- Returns true iff the list of cards are continuous and visible
isContinuous :: [Card] -> Bool
isContinuous [] = True
isContinuous [(_,up)] = up
isContinuous ((val,up):(nval,nup):r) = up && ((val + 1) == nval) && (isContinuous ((nval,nup):r))

-- given the state of the game, and the target position of the card, return true if the card can be moved, and false otherwise
-- assume the position is valid
-- the card can be moved if: 
--      it is the first one of one pile from bottom up (the first elem in the list)
--      all the cards before it (in the list) are continuous
canChoose :: InternalState -> Position -> Bool
-- canChoose game (column, row)
--     | row == 0 = True
--     | otherwise = trace ("display: " ++ show list) comparePile list 0 row 
--     where 
--         list = getColumn (piles game) column
canChoose state (col,row) =
  let cards = (piles state) !! col
  in (row < (length cards)) && isContinuous (take (row+1) cards)

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