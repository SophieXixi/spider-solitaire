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

data CardMove = CardMove {
  from :: Position,
  to :: Pile
}
  deriving Show


cardWidth :: Float
cardWidth = 80

cardHeight :: Float
cardHeight = 120

cardSpacing :: Float
cardSpacing = 30

pileSpacing :: Float
pileSpacing = 20

pileWidth :: Float
pileWidth = 80

totalWidth :: Float
totalWidth = 10 * (pileWidth + pileSpacing) - pileSpacing

windowWidth :: Float
windowWidth = 1200

windowHeight :: Float
windowHeight = 800

pileStartX :: Float
pileStartX = -(windowWidth / 2) + pileSpacing

pileStartY :: Float
pileStartY = windowHeight / 2 - cardHeight - pileSpacing

deckWidth :: Float
deckWidth = 80

deckHeight :: Float
deckHeight = 120

deckX :: Float
deckX = -540

deckY :: Float
deckY = 320

startX :: Float
startX = -(totalWidth / 2) + pileSpacing / 2

startY :: Float
startY = 150 + cardHeight / 2


