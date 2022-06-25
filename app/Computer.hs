module Computer (
    playNextMove
) where

{----------------------------------------------------
Importing modules
-----------------------------------------------------}

import Data.Bits            ( xor )

-- local modules
import Model

{----------------------------------------------------
Code
-----------------------------------------------------}

-- Compute the binary sum of sticks quantities present on the game board
nimSum :: Board -> Int
nimSum b = foldr1 xor (map snd b)

-- It is possible to guess which player will "theorically" win from the state of the game board
-- If nimSum is null, the player who plays first will lose unless the other player makes a mistake
-- In order to win, player has to make a move which yields the nimSum to zero.
guessWinnerBeforePlaying :: Board -> Player
guessWinnerBeforePlaying b
    | nimSum b == 0 = 2
    | otherwise     = 1

-- Computer generates a move based on the current game board
-- If nimSum is not null, computer will make a move yielding the nimSum to zero
-- If nimSum is null, computer will make an arbitrary move
playNextMove :: Board -> Move
playNextMove b
    | nimSum b /= 0 = (fst targetRow, snd targetRow - targetSticks)
    | otherwise     = (fst maxSticks, if null canRemove then 1 else minimum canRemove)
    where
        sts          = map snd b
        nsum         = nimSum b
        targetRow    = head $ filter (\(_,y) -> y > xor nsum y) b
        targetSticks = xor nsum (snd targetRow)
        maxSticks    = head $ filter (\(_,y) -> y == maximum sts) b
        canRemove    = [x | x <- [1..snd maxSticks - 1], snd maxSticks - x `notElem` sts ]


