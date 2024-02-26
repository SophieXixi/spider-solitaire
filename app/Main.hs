module Main where

import GUI
import Definition
import Action

main :: IO ()
main = mainGUI

-- main :: IO ()
-- main = do
--   game <- generateInitialState
--   let state = gameState game
--   displayState state
--   putStrLn ("Possible Moves:" ++ show (getPossibleCardMoves (piles state)))
--   putStrLn ("Current Value:" ++ show (getValueOfState state))
--   putStrLn ("Suggested Move:" ++ show (suggestNextCardMove state))
--   putStrLn ("Auto-Solve: " ++ show (solveGame state))