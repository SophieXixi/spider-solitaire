module Action where

import Definition

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
                    finishSet = finishSet state,
                    position = position state
                }
    
distributedCards:: [Card] -> [[Card]] -> [[Card]]
distributedCards [] _ = []
distributedCards _ [] = []
distributedCards ((num, visible):t) (hp:tp) = ((num, True):hp) : distributedCards t tp

-- canChoose :: State -> Position -> Bool

-- getAvailableActions :: Action -> InternalState -> [Action]
