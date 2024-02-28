module GUI where

-- cabal install --lib gloss
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Definition
import Action
import Data.Maybe


data GUIResources = GUIResources {
  cardImages :: [Picture],
  cardBackImage :: Picture
}

imageFilePaths = [ "resources/spades_" ++ show i ++ ".bmp" | i <- [1..13] ]


-- Specify the content on a card
showRank :: Int -> String
showRank rank
  | rank == 1 = "A"
  | rank > 1 && rank < 11 = show rank
  | rank == 11 = "J"
  | rank == 12 = "Q"
  | rank == 13 = "K"
  | otherwise = error "Invalid rank"

-- Show a card
drawCard :: GUIResources -> Card -> Picture
drawCard resources (rank, faceUp) =
  if faceUp then
    pictures [
      scale 0.4 0.4 (cardImages resources !! (rank - 1)),
      color borderColor $ rectangleWire cardWidth cardHeight
    ]
  else
    pictures [
      scale 0.11 0.11 (cardBackImage resources),
      color borderColor $ rectangleWire cardWidth cardHeight
    ]
  where
    cardColor = light (light blue)
    borderColor = dark $ dim blue
    backColor = greyN 0.5
    textColor = black

-- show the piles picture
drawPiles :: GUIResources -> InternalState -> Picture
drawPiles resources state = pictures $ zipWith (curry drawPileWithIndex) [0..] (piles state)
  where
    drawPileWithIndex :: (Int, [Card]) -> Picture
    drawPileWithIndex (index, pile) =
      let pilesStartX = fromIntegral index * (pileWidth + pileSpacing) - totalWidth / 2 + pileWidth / 2 + pileSpacing / 2
      in if null pile
         then translate pilesStartX 150 $ color black $ rectangleWire pileWidth cardHeight
         else translate pilesStartX 150 $ pictures $ zipWith drawCardOffset (reverse pile) [0..]

    drawCardOffset :: Card -> Int -> Picture
    drawCardOffset card index = translate 0 (-fromIntegral index * cardSpacing) $ drawCard resources card

-- show the deck picture
drawDeck :: GUIResources -> InternalState -> Picture
drawDeck resources state = translate deckX deckY $ pictures [deckPicture, deckCountText]
  where
    -- deckPicture = color deckColor $ rectangleSolid deckWidth deckHeight
    deckPicture = scale 0.115 0.115 $ cardBackImage resources
    deckCountText = translate (-8) (-10) $ scale 0.2 0.2 $ color textColor $ text (show $ length (remaining state))
    deckColor = dark green
    textColor = white

-- show the number of completed decks
drawFinishSetInfo :: InternalState -> Picture
drawFinishSetInfo state = translate x y $ scale 0.15 0.15 $ color textColor $ text finishSetText
  where
    finishSetText = "Completed: " ++ show (finishedSets state)
    textColor = black
    x = -windowWidth / 2 + 120
    y = windowHeight / 2  - 80

-- show win message
drawWinMessage :: InternalState -> Picture
drawWinMessage state = if finishedSets state == totalSets
                       then translate (-windowWidth / 4) 0 $ scale 0.3 0.3 $ color winMessageColor $ text "Congratulations!!! You win!"
                       else Blank
  where
    winMessageColor = red
    totalSets = 8

-- Button for AI solving
drawButtonForAI :: Picture
drawButtonForAI = pictures [botton, bottonText]
  where
    botton = translate buttonX1 buttonY1 $ color buttonColor $ rectangleSolid weight1 height1
    bottonText = translate (buttonX1 - 10) (buttonY1 - 10) $ scale 0.2 0.2 $ color black $ text "AI"
    buttonColor = light (light blue)

-- Button for hiti
drawButtonForHint :: Picture
drawButtonForHint = pictures [botton, bottonText]
  where
    botton = translate buttonX2 buttonY2 $ color buttonColor $ rectangleSolid weight2 height2
    bottonText = translate (buttonX2 - 20) (buttonY2 - 10) $ scale 0.2 0.2 $ color black $ text "Hint"
    buttonColor = orange

-- show position info
drawSelectionInfo :: InternalState -> Picture
drawSelectionInfo state =
  translate (-590) (-380) $ scale 0.15 0.15 $ color black $ text selectionText
  where
    selectionText = case position state of
      Just (pileIndex, cardIndex) ->
        let card = (piles state !! pileIndex) !! cardIndex
        in "Selected card: " ++ showCard card
      Nothing -> "Selected: Nothing"

-- show card status
showCard :: Card -> String
showCard (rank, faceUp) = if faceUp then showRank rank else "Face Down"

-- game status
drawGame :: GUIResources -> InternalState -> Picture
drawGame resources state = pictures [drawPiles resources state, drawDeck resources state, drawFinishSetInfo state, drawWinMessage state, drawSelectionInfo state, drawButtonForAI, drawButtonForHint]

-- mouse click event
handleEvent :: Event -> State -> State
handleEvent event state@(State { gameState = internalState }) =
  case event of
    EventKey (MouseButton LeftButton) Down _ mousePos ->
      if clickOnHintButton mousePos then
        case suggestNextCardMove internalState of
          Just move -> state { gameState = moveCardsAlt internalState move }
          Nothing -> state
      else if clickOnAIButton mousePos then
        case solveGameMemRun internalState of
          Just solution -> state { gameState = applySolution internalState solution }
          Nothing -> state
      else if clickOnDealPile mousePos && not (null (remaining internalState)) then
          state { gameState = dealCards internalState }
      else
        case clickOnCard mousePos internalState of
          Just pos ->
            if isNothing (position internalState) then
                state { gameState = selectCard internalState pos }
            else
              case position internalState of
                  Just oldPos ->
                    if canActionBePerformed internalState (Move (fst pos))
                      then state { gameState = (moveCards internalState oldPos (fst pos)) { position = Nothing } }
                      else state { gameState = internalState { position = Nothing }}
                  Nothing -> state
          Nothing -> state
    _ -> state

-- select a card (which means change Position)
selectCard :: InternalState -> Position -> InternalState
selectCard state pos =
    if canChoose state pos then state { position = Just pos } else state

-- click On DealPile or not
clickOnDealPile :: Point -> Bool
clickOnDealPile (clickX, clickY) =
  clickX >= deckX - halfDeckWidth && clickX <= deckX + halfDeckWidth &&
  clickY <= deckY + halfDeckHeight && clickY >= deckY - halfDeckHeight
  where
    halfDeckWidth = deckWidth / 2
    halfDeckHeight = deckHeight / 2

-- click on a card, but make sure you only click the top of the card can 
clickOnCard :: Point -> InternalState -> Maybe Position
clickOnCard (clickX, clickY) state = do
  let pilesWithIndex = zip [0..] (piles state)

  findClickPosition pilesWithIndex clickX clickY

-- get Click Position
findClickPosition :: [(Int, [Card])] -> Float -> Float-> Maybe Position
findClickPosition [] _ _ = Nothing
findClickPosition ((index, pile):rest) clickX clickY =
  if inPileXRange clickX index then
    if null pile then Just (index, 0)
    else
      let cardIndex = findCardIndex clickY pile 0
      in case cardIndex of
          Just ci -> Just (index, ci)
          Nothing -> findClickPosition rest clickX clickY
  else findClickPosition rest clickX clickY

-- check whether the clicked X coordinate is within the X range of a certain deck
inPileXRange :: Float -> Int -> Bool
inPileXRange clickX index =
  let pileXStart = startX + fromIntegral index * (pileWidth + pileSpacing)
  in clickX >= pileXStart && clickX <= pileXStart + pileWidth

-- check whether the clicked Y coordinate is within the Y range of a certain deck
findCardIndex :: Float -> [Card] -> Int -> Maybe Int
findCardIndex clickY pile index =
  let totalCards = length pile
      cardTopY = startY - fromIntegral index * cardSpacing
  in if index >= totalCards then Nothing
      else if clickY >= (if index == (totalCards - 1) then cardTopY - cardHeight else cardTopY - cardSpacing) && clickY < cardTopY
          then Just (totalCards - index - 1)
      else findCardIndex clickY pile (index + 1)

-- check if click on the AI button
clickOnAIButton :: Point -> Bool
clickOnAIButton (clickX, clickY) =
  clickX >= (buttonX1 - weight1/2) && clickX <= (buttonX1 + weight1/2) && clickY >= (buttonY1 - height1/2) && clickY <= (buttonY1 + height1/2)

-- check if click on the hiti button
clickOnHintButton :: Point -> Bool
clickOnHintButton (clickX, clickY) =
  clickX >= (buttonX2 - weight2/2) && clickX <= (buttonX2 + weight2/2) && clickY >= (buttonY2 - height2/2) && clickY <= (buttonY2 + height2/2)

-- only accept data of InternalState type
applySolution :: InternalState -> [Maybe CardMove] -> InternalState
applySolution state [] = state
applySolution state (move:rest) =
  case move of
    Just cardMove ->
      let newState = performCardMove state cardMove
      in applySolution newState rest
    Nothing ->
      let newState = dealCards state
      in applySolution newState rest

loadCardImages :: [FilePath] -> IO [Picture]
loadCardImages [] = do return []
loadCardImages (h:t) = do
  img <- loadBMP h
  restImg <- loadCardImages t
  return (img:restImg)

-- run the app
mainGUI :: IO ()
mainGUI = do
  initialState <- generateInitialState
  cBack <- loadBMP "resources/blue.bmp"
  cImages <- loadCardImages imageFilePaths
  let resources = GUIResources {
    cardImages = cImages,
    cardBackImage = cBack
  }
  play window background fps initialState (render resources) handleEvent update
  where
      window = InWindow "Spider Solitaire" (1200, 800) (100, 100)
      background = dark $ dark $ dim green
      fps = 30

-- draw the game state
render :: GUIResources -> State -> Picture
render resources state = drawGame resources (gameState state)

-- auto update function
update :: Float -> State -> State
update _ state =
    state { gameState = tryToCompleteFoundationPile (gameState state) }

