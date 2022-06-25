module Main where

{----------------------------------------------------
Game rules:
    - 2 players game
    - Board is composed of N rows of M matchsticks (M variable for each row)
    - Player could remove any number of matchsticks from a single row only
    - Winner is the player who removes the last matchsticks
-----------------------------------------------------}

{----------------------------------------------------
Game configuration:
    - At start, it has been asked to define the desired configuration for the game although default settings are being proposed
    - Settings: nb of rows (minimum is 3), composition of rows (nb of sticks for each row), character symbol to use for displaying a single stick on the game board
    - An option has been added to allow playing against computer. In that case, computer will be Player 2.
-----------------------------------------------------}

{----------------------------------------------------
Importing modules
-----------------------------------------------------}
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import System.Console.ANSI  ( clearScreen )

-- local modules
import Configuration
import Game


{----------------------------------------------------
Main
-----------------------------------------------------}

main :: IO ()
main = do
    -- Clear screen
    clearScreen
    putStrLn "---------------------------------------------"
    putStrLn "------------ Welcome to NIM game ------------"
    putStrLn "---------------------------------------------\n"

    -- Initialize configuration from user input
    config <- initConfig

    -- Initialize game from configuration
    let game = runReader initGame config

    -- Clear screen
    clearScreen

    -- Play the game and collect history of player's moves
    (_, history) <- runWriterT (runStateT (runReaderT playGame config) game)

    -- Print the history of moves
    putStrLn "\nHistory of moves:"
    putStrLn history

    -- Waiting for [enter] key to be pressed for exiting the program
    putStrLn "\nPress [enter] to quit..."
    getLine
    return ()
