module Main where

data State = State InternalState [Action]  -- internal_state available_actions
         deriving (Ord, Eq, Show)

data Result = EndOfGame Double State    -- end of game: value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> Action

-- Spider

type Card = (Int, Bool)

-- data Card = Card Int Bool

type Pile = Int

type Position = (Pile, Int) -- (which pile, which index in pile)

-- arguments: 10 piles, reserve, number of foundation piles completed, current selected card position
data InternalState = InternalState [[Card]] [[Card]] Int (Maybe Position)

data Action = Choose Position
            | Move Pile
            | Deal


--- Helper methods

-- generateInitialState :: State

-- tryToCompleteFoundationPile :: InternalState -> InternalState

-- -- first Position is from position, second Pile is target pile
-- moveCards :: InternalState -> Position -> Pile -> InternalState


-- -- 3 methods above to be completed by William
-- -- 3 methods below to be completed by Sophie


-- dealCards :: InternalState -> InternalState

-- canChoose :: State -> Position -> Bool

-- getAvailableActions :: Action -> InternalState -> [Action]
