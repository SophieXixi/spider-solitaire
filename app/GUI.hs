module GUI where

-- cabal install --lib gloss
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Definition
import Action

showRank :: Int -> String
showRank rank
  | rank == 1 = "A"
  | rank > 1 && rank < 11 = show rank
  | rank == 11 = "J"
  | rank == 12 = "Q"
  | rank == 13 = "K"
  | otherwise = error "Invalid rank"

drawCard :: Card -> Picture
drawCard (rank, faceUp) =
  if faceUp then
    pictures [
      color cardColor $ rectangleSolid width height,
      color borderColor $ rectangleWire width height,
      translate (-width * 0.5 + 10) (height * 0.5 - 20) $ scale 0.15 0.15 $ color textColor $ text (showRank rank)
    ]
  else
    pictures [
      color backColor $ rectangleSolid width height,
      color borderColor $ rectangleWire width height
    ]
  where
    width = 80
    height = 120
    cardColor = light blue
    borderColor = black
    backColor = greyN 0.5
    textColor = black

drawPiles :: InternalState -> Picture
drawPiles state = pictures $ map drawPileWithIndex (zip [0..] (piles state))
  where
    pileSpacing = 20
    cardSpacing = 30
    pileWidth = 80

    drawPileWithIndex :: (Int, [Card]) -> Picture
    drawPileWithIndex (index, pile) = translate x 150 $ pictures $ zipWith drawCardOffset (reverse pile) [0..]
      where
        x = fromIntegral index * (pileWidth + pileSpacing) - totalWidth / 2 + pileWidth / 2 + pileSpacing / 2

    drawCardOffset :: Card -> Int -> Picture
    drawCardOffset card index = translate 0 (-fromIntegral index * cardSpacing) $ drawCard card

    totalWidth = fromIntegral (length (piles state)) * (pileWidth + pileSpacing) - pileSpacing

drawDeck :: InternalState -> Picture
drawDeck state = translate deckX deckY $ pictures [deckPicture, deckCountText]
  where
    deckPicture = color deckColor $ rectangleSolid deckWidth deckHeight
    deckCountText = translate (-deckWidth * 0.15) (-10) $ scale 0.2 0.2 $ color textColor $ text (show $ length (remaining state))
    deckX = -windowWidth / 2 + deckWidth / 2 + margin
    deckY = windowHeight / 2 - deckHeight / 2 - margin
    deckColor = dark green
    textColor = white
    deckWidth = 80
    deckHeight = 120
    margin = 20
    windowWidth = 1200
    windowHeight = 800

drawFinishSetInfo :: InternalState -> Picture
drawFinishSetInfo state = translate x y $ scale 0.15 0.15 $ color textColor $ text finishSetText
  where
    finishSetText = "Completed: " ++ show (finishedSets state)
    textColor = black
    x = -windowWidth / 2 + deckWidth + margin * 2 + 20
    y = windowHeight / 2 - margin - 50
    deckWidth = 80
    margin = 20
    windowWidth = 1200
    windowHeight = 800


drawWinMessage :: InternalState -> Picture
drawWinMessage state = if finishedSets state == totalSets 
                       then translate (-windowWidth / 4) 0 $ scale 0.25 0.25 $ color winMessageColor $ text "Congratulations!!! You win！"
                       else Blank
  where
    winMessageColor = green
    totalSets = 8
    windowWidth = 1200

handleEvent :: Event -> State -> State
handleEvent event state =
    case event of
        EventKey (MouseButton LeftButton) Up _ mousePos ->
            if clickOnDealPile mousePos then
                state { gameState = dealCards (gameState state) }
            else
                state
        _ -> state

clickOnDealPile :: (Float, Float) -> Bool
clickOnDealPile (x, y) = 
    x >= deckX - halfDeckWidth && x <= deckX + halfDeckWidth &&
    y <= deckY + halfDeckHeight && y >= deckY - halfDeckHeight
  where
    deckX = -500
    deckY = 300
    halfDeckWidth = deckWidth / 2
    halfDeckHeight = deckHeight / 2
    deckWidth = 80
    deckHeight = 120

drawGame :: InternalState -> Picture
drawGame state = pictures [drawPiles state, drawDeck state, drawFinishSetInfo state, drawWinMessage state]


mainGUI :: IO ()
mainGUI = do
    initialState <- generateInitialState
    play window background fps initialState render handleEvent update
    where
        window = InWindow "Spider Solitaire" (1200, 800) (100, 100)
        background = white
        fps = 30

render :: State -> Picture
render state = drawGame (gameState state)

update :: Float -> State -> State
update _ state =
    state { gameState = tryToCompleteFoundationPile (gameState state) }
