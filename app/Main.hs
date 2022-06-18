{----------------------------------------------------
Language extensions
-----------------------------------------------------}
{-# LANGUAGE FlexibleContexts #-}

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

import Data.List            (intersperse)
import Data.Bits            (xor)
import Text.Printf          (printf)
import Text.Read            hiding (lift, get)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad        (when)

{----------------------------------------------------
Constants
-----------------------------------------------------}

cNbRows     :: NbRows
cNbRows     = 3           -- default nb of rows being proposed when asked for desired configuration
cListSticks :: [Int]
cListSticks = [9, 16, 13] -- default composition of rows being proposed when asked for desired configuration
cSymbol     :: String
cSymbol     = "!"         -- default character symbol being proposed when asked for desired configuration

{----------------------------------------------------
Data Model
-----------------------------------------------------}

type NbRows      = Int    -- nb of rows
type ListSticks  = [Int]  -- configuration of rows
type StickSymbol = String -- character symbol representing a stick to be shown when displaying the game board

data PlayMode    = COMPUTER | HUMAN deriving (Eq, Show)

-- Reader Config

data Config = Config {          -- Game configuration
      _nbRows     :: NbRows
    , _listSticks :: ListSticks
    , _symbol     :: StickSymbol
    , _mode       :: PlayMode
    } deriving (Show)

----------------------------------------------------

type RowNo    = Int           -- row number
type NbSticks = Int           -- quantity of sticks

type Row    = (RowNo, NbSticks) -- a row is a tuple (row number starting from 1, quantity of sticks left in that row)
type Board  = [Row]             -- list of rows
type Player = Int               -- current player : 1 or 2
type Move   = (RowNo, NbSticks) -- a move is a tuple (row number, quantity of sticks to remove)

-- State Game

data Game = Game {
      _board  :: Board
    , _player :: Player
    , _move   :: Move
    } deriving (Show)

{----------------------------------------------------
Pure Functions
-----------------------------------------------------}

-- Create a Board from (NbRows, ListSticks)
createBoard :: (NbRows, ListSticks) -> Board
createBoard (n, ss) = zip [1 .. n] ss

-- Remove NbSticks sticks from Row
changeRow :: (Row, NbSticks) -> Row
changeRow ((r,m), n) = (r, m-n)

-- Reflect board change following a player's move
applyMove :: Game -> Game
applyMove (Game (r:rs) p (1, n)) = Game {_board = changeRow (r, n) : rs                      , _player = p, _move = (1, n)}
applyMove (Game (r:rs) p (i, n)) = Game {_board = r : _board (applyMove (Game rs p (i-1, n))), _player = p, _move = (i, n)}
applyMove g                      = g

-- Try to convert user's input into a well-formed move
parsePlayerMove :: String -> Maybe Move
parsePlayerMove s
    | length (filter (== ' ') s) /= 1 = Nothing
    | otherwise                       = do
        let w = words s
        r <- readMaybe $ head w :: Maybe RowNo
        n <- readMaybe $ last w :: Maybe NbSticks
        Just (r, n)

-- Check whether player's entered move is valid in regard to the current board state
isMoveValid :: (MonadState Game m) => Move -> m Bool
isMoveValid move = do
    game <- get
    let
        b        = _board game
        (ro, st) = move
    if ro < 1 || st < 1 || ro > length b
        then return False
        else return $ st <= snd (b !! (ro-1))

-- Check whether game is over (that is board is empty)
isGameOver :: (MonadState Game m) => m Bool
isGameOver =
    gets (all ((== 0) . snd) . _board)
    -- same as : all ((== 0) . snd) . _board <$> get

initGame :: Reader Config Game
initGame = do
    config <- ask
    return $ Game {
          _board  = createBoard (_nbRows config, _listSticks config)
        , _player = 1
        , _move   = (0,0)
        }

{----------------------------------------------------
Extra code for playing against computer
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
playNextMove :: (MonadState Game m) => m Move
playNextMove = do
    game <- get

    let
        b            = _board game
        sts          = map snd b
        nsum         = nimSum b
        targetRow    = head $ filter (\(_,y) -> y > xor nsum y) b
        targetSticks = xor nsum (snd targetRow)
        maxSticks    = head $ filter (\(_,y) -> y == maximum sts) b
        canRemove    = [x | x <- [1..snd maxSticks - 1], snd maxSticks - x `notElem` sts ]

    if nimSum b /= 0
        then return (fst targetRow, snd targetRow - targetSticks)
        else return (fst maxSticks, if null canRemove then 1 else minimum canRemove)

-- Helper function to determine whether it is computer's turn
-- Computer is Player 2 when COMPUTER play mode is being choosen
isComputerTurn :: (MonadReader Config m, MonadState Game m) => m Bool
isComputerTurn = do
    config <- ask
    game   <- get
    return $ _mode config == COMPUTER && _player game == 2

{----------------------------------------------------
Impure Functions
-----------------------------------------------------}

-- Set configuration from user input

-- Ask user to enter the number of rows the game board is composed of or to accept the default
selectNbRows :: IO NbRows
selectNbRows = do
    putStrLn $ "How many rows (greather than 2) does the board game have? Enter an integer value or press [enter] for default ( = " ++ show cNbRows ++ " )"
    input <- getLine
    if input == ""
        then return cNbRows -- default value defined in constants section
        else
            case readMaybe input of
                Nothing -> selectNbRows
                Just v  -> if v >= cNbRows then return v else selectNbRows

-- Ask user to enter the composition of rows or to accept the default
selectListSticks :: IO ListSticks
selectListSticks = do
    putStrLn $ "How many sticks does each row have? Enter a list of integer values or press [enter] for default ( = " ++ show cListSticks ++ " )"
    input <- getLine
    if input == ""
        then return cListSticks -- default value defined in constants section
        else
            maybe selectListSticks return (readMaybe input)

-- Ask user to enter the stick symbol or to accept the default
selectStickSymbol :: IO StickSymbol
selectStickSymbol = do
    putStrLn $ "Which symbol to use to show as a stick? Enter a single character or press [enter] for default ( = " ++ cSymbol ++ " )"
    input <- getLine
    if input == ""
        then return cSymbol -- default value defined in constants section
        else if length input == 1
            then return input
            else selectStickSymbol

-- Ask user to enter the play mode or to accept the default
selectPlayMode :: IO PlayMode
selectPlayMode = do
    putStrLn "Want to play against computer? Select 1 or 2 or press [enter] for default ( = against computer )"
    putStrLn "1 = You (Player 1) play against Computer (Player 2)"
    putStrLn "2 = You (Player 1) play against Human (Player 2)"
    input <- getLine
    case input of
        ""  -> return COMPUTER
        "1" -> return COMPUTER
        "2" -> return HUMAN
        _   -> selectPlayMode

-- Set configuration from user's choice
initConfig :: IO Config
initConfig = do
    nbRows <- selectNbRows
    sticks <- selectListSticks
    if length sticks /= nbRows
        then do
            putStrLn "Incorrect configuration"
            initConfig
        else do
            symbol <- selectStickSymbol
            mode   <- selectPlayMode
            return $ Config {
                  _nbRows     = nbRows
                , _listSticks = sticks
                , _symbol     = symbol
                , _mode       = mode
                }

-- Display board

showHorizontalLine :: IO ()
showHorizontalLine = putStrLn $ replicate 50 '-'

showRow :: (Row, StickSymbol) -> IO ()
showRow ((r, m), s) = do
    showHorizontalLine
    putStr $ "| " ++ show r ++ " ==> "++ printf "%02d" m ++ " | "
    putStrLn $ intersperse ' ' (replicate m (head s))

showBoard :: (MonadReader Config m, MonadState Game m, MonadIO m) => m ()
showBoard = do
    config <- ask
    game   <- get

    liftIO $ do
        mapM_ showRow ((,) <$> _board game <*> [_symbol config])
        showHorizontalLine
        putStrLn ""

-- Capture user's move

getPlayerMove :: (MonadState Game m, MonadIO m) => m (Maybe Move)
getPlayerMove = do
    game <- get

    liftIO $ putStrLn $ "Player " ++ show (_player game) ++ ", Enter your move (row number <whitespace> number of sticks to remove) :"
    input <- liftIO getLine

    return $ parsePlayerMove input

-- Display notifications

notifyInvalidMove :: IO ()
notifyInvalidMove = putStrLn "\nInvalid input for move ! Please try again."

notifyIncorrectMove :: IO ()
notifyIncorrectMove = putStrLn "\nIncorrect move: move cannot be played in regard to the board game ! Please try again."

notifyWinner :: (MonadState Game m, MonadIO m) => m ()
notifyWinner = do
    game <- get
    liftIO $ putStrLn $ "\nCongratulations! Player " ++ show (_player game) ++ " - You won !!"

-- Play game and analyze move

-- Main function of the program
-- Use of ReaderT to read configuration stored in Config
-- Use of StateT to manage the state of the game stored in Game
-- Use of WriterT to log history of players' moves
playGame :: ReaderT Config (StateT Game (WriterT String IO)) ()
playGame = do
    config <- ask
    game   <- get

    -- Display game board
    showBoard

    -- Who's turn? Computer or Player?
    computerPlays <- isComputerTurn
    if not computerPlays
        then do -- Player's turn
            -- Get player's move from input
            mm <- getPlayerMove
            case mm of
                Nothing -> do
                    -- Notify invalid input
                    liftIO notifyInvalidMove
                    playGame
                Just move -> do
                    -- Input is well-formed but is the move valid in regard to board state
                    validMove <- isMoveValid move
                    if not validMove
                        then do
                            -- Notify incorrect move
                            liftIO notifyIncorrectMove
                            playGame
                        else do
                            -- Process player's move and continue if game is not over
                            continue <- processMove move
                            Control.Monad.when continue playGame
        else do -- Computer's turn
            -- Process computer's move and continue if game is not over
            nextMove <- playNextMove
            continue <- processMove nextMove
            Control.Monad.when continue playGame

processMove :: (MonadState Game m, MonadWriter String m, MonadIO m) => Move -> m Bool
processMove move = do
    game <- get

    -- Describe move
    describePlayerMove move

    -- Log initial state and current move
    logPlayerMove move

    -- Change state by applying the move
    playMove move

    -- Final state or switch player and continue game?
    game <- get
    gameOver <- isGameOver
    if gameOver
        then do
            notifyWinner
            return False
        else do
            switchPlayer
            return True

describePlayerMove :: (MonadState Game m, MonadIO m) => Move -> m ()
describePlayerMove move = do
    game <- get
    let
        player = _player game
        ro     = fst move
        st     = snd move
    liftIO $ putStrLn $ "\nPlayer " ++ show player ++ " removed " ++ show st ++ " stick(s) from row " ++ show ro

logPlayerMove :: (MonadState Game m, MonadWriter String m) => Move -> m ()
logPlayerMove move = do
    game <- get
    let
        board  = _board game
        player = _player game
        ro     = fst move
        st     = snd move
    tell $ "Board: " ++ show board ++ " - Move: (" ++ show ro ++ ", " ++ show st ++ ") -> Player " ++ show player ++ " removed " ++ show st ++ " stick(s) from row " ++ show ro ++ "\n"

-- Change state in regard to player's move
playMove :: (MonadState Game m) => Move -> m ()
playMove move = do
    game <- get

    -- Register player's move
    put $ game {_move = move}

    -- Update board to reflect the move
    modify applyMove

-- Switch player's turn
switchPlayer :: (MonadState Game m) => m ()
switchPlayer = do
    game <- get
    put $ game { _player = if _player game == 1 then 2 else 1 }

{----------------------------------------------------
Main
-----------------------------------------------------}

main :: IO ()
main = do
    putStrLn "\n---------------------------------------------"
    putStrLn "------------ Welcome to NIM game ------------"
    putStrLn "---------------------------------------------\n"

    -- Initialize configuration from user input
    config <- initConfig

    -- Initialize game from configuration
    let game = runReader initGame config

    -- Play the game and collect history of player's moves
    (_, history) <- runWriterT (runStateT (runReaderT playGame config) game)

    -- Print the history of moves
    putStrLn "\nHistory of moves:"
    putStrLn history

    -- Waiting for [enter] key to be pressed for exiting the program
    putStrLn "\nPress [enter] to quit..."
    getLine
    return ()
