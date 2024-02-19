module Main where
import Definition
import Action

countRemaining:: [[Card]] -> Int
countRemaining lst = length (filter (not . null) lst)

displayPiles:: [Card] -> [Char]
displayPiles [] = ['\n']
displayPiles ((num, visible):t)
   | visible = show num ++ " " ++ displayPiles t
   | otherwise = "* " ++ displayPiles t

display:: State -> IO ()
display game = do
   let rem = "remaining: " ++ show (countRemaining(remaining (gameState game)))
   let cards = "piles: \n" ++ concatMap displayPiles (piles (gameState game))
   putStrLn rem
   putStrLn cards

main:: IO ()
main = do 
   putStrLn "welcome!"
   let initGame = State { gameState = initg, actions = [] }
   finalstate <- play initGame
   putStrLn "game over!"

play:: State -> IO State
play game = 
   do
      display game
      putStrLn "enter an action"
      act <- getLine
      case act of
         "deal" -> do
            let newstate = dealCards (gameState game)
            putStrLn "dealing cards"
            play (game { gameState = newstate})
         _ -> do
            putStrLn "invalid move"
            return game