module Definition where

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
