module Definition where

data State = State {
    gameState:: InternalState,       -- internal_state
    actions:: [Action]          -- available_actions
    }

data Result = EndOfGame Double State    -- end of game: value, starting state
            | ContinueGame State        -- continue with new state

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
-- finishSet: the cards piled up from left to right
data InternalState = InternalState
    { remaining :: [[Card]]
    , piles :: [[Card]]
    , finishSet :: [Bool]
    , position :: Maybe Position
    } deriving (Show)


data Action = Choose Position
            | Move Pile
            | Deal




initg :: InternalState
initg = InternalState
    { remaining =
        [ [(6, False), (11, False), (2, False), (12, False), (10, False), (12, False), (13, False), (3, False), (10, False), (1, False)]
        , [(8, False), (13, False), (2, False), (8, False), (12, False), (4, False), (7, False), (5, False), (13, False), (1, False)]
        , [(9, False), (13, False), (13, False), (1, False), (13, False), (5, False), (6, False), (11, False), (4, False), (4, False)]
        , [(9, False), (3, False), (9, False), (10, False), (4, False), (8, False), (2, False), (9, False), (11, False), (7, False)]
        , [(7, False), (4, False), (9, False), (1, False), (1, False), (5, False), (2, False), (1, False), (5, False), (12, False)]
        ]
    , piles =
        [ [(10, True), (11, False), (9, False), (11, False), (5, False), (3, False)]
        , [(6, True), (7, False), (3, False), (6, False), (3, False), (10, False)]
        , [(5, True), (7, False), (3, False), (12, False), (8, False), (4, False)]
        , [(9, True), (13, False), (2, False), (8, False), (6, False), (2, False)]
        , [(10, True), (9, False), (12, False), (13, False), (3, False)]
        , [(2, True), (6, False), (2, False), (1, False), (7, False)]
        , [(12, True), (3, False), (6, False), (5, False), (4, False)]
        , [(12, True), (6, False), (11, False), (8, False), (7, False)]
        , [(8, True), (1, False), (7, False), (4, False), (5, False)]
        , [(8, True), (10, False), (11, False), (10, False), (11, False)]
        ]
    , finishSet = [False, False, False, False, False, False, False, False]
    }

resGame = InternalState {
            remaining = [[], [], [], [], []], 
            piles = [[], [], [], [], [], [], [], [], [], []],
            finishSet = [True, True, True, True, True, True, True, True]
            }

