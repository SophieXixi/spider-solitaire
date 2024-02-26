module Main where

import GUI
import Definition
import Action
import Data.Maybe

main :: IO ()
main = mainGUI

{-
main :: IO ()
main = do
  game <- generateInitialState
  let state = gameState game
  displayState state
  putStrLn ("Complete: \n" ++ showComplete state)
  -- putStrLn ("Condensed: " ++ showCondensed state)
  -- putStrLn ("Possible Moves: " ++ show (getPossibleCardMoves (piles state)))
  -- putStrLn ("Current Value: " ++ show (getValueOfState state))
  -- putStrLn ("Suggested Move: " ++ show (suggestNextCardMove state))
  -- let solution = solveGame state
  -- let solution = solveGameMemRun state
  -- putStrLn ("Length of solution: " ++ (if isNothing solution then "None" else show (length (fromJust solution))))
  -- putStrLn ("Solution: " ++ show solution)
  putStrLn ("Solution (with mem): " ++ showSolution (solveGameMemRun state))
-}