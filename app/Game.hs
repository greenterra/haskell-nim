{----------------------------------------------------
Language extensions
-----------------------------------------------------}
{-# LANGUAGE FlexibleContexts #-}

module Game where

{----------------------------------------------------
Importing modules
-----------------------------------------------------}

import Data.List            ( intersperse )
import Text.Printf          ( printf )
import Text.Read            hiding ( lift, get )
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad        ( when )
import System.Console.ANSI  ( clearScreen )

-- local modules
import Model
import Configuration
import Computer

{----------------------------------------------------
Data Functions
-----------------------------------------------------}

-- Initialize game state with board derived from configuration and player 1 plays first
initGame :: Reader Config Game
initGame = do
    config <- ask
    return $ Game {
          _board  = createBoard (_nbRows config, _listSticks config)
        , _player = 1
        , _move   = (0,0)                                            -- dummy setting for compliance with full data initialization
        }

-- Create a Board from (NbRows, ListSticks)
createBoard :: (NbRows, ListSticks) -> Board
createBoard (n, ss) = zip [1 .. n] ss

{----------------------------------------------------
Transformation Functions
-----------------------------------------------------}

-- Remove NbSticks sticks from Row
changeRow :: (Row, NbSticks) -> Row
changeRow ((r,m), n) = (r, m-n)

-- Reflect board change following a player's move
applyMove :: Game -> Game
applyMove (Game (r:rs) p (1, n)) = Game {_board = changeRow (r, n) : rs                      , _player = p, _move = (1, n)}
applyMove (Game (r:rs) p (i, n)) = Game {_board = r : _board (applyMove (Game rs p (i-1, n))), _player = p, _move = (i, n)}
applyMove g                      = g -- useless but for compliance with exhaustive pattern matching

{----------------------------------------------------
Helper Functions
-----------------------------------------------------}

-- Helper function to determine whether it is computer's turn
-- Computer is Player 2 when COMPUTER play mode is being choosen
isComputerTurn :: (MonadReader Config m, MonadState Game m) => m Bool
isComputerTurn = do
    config <- ask
    game   <- get
    return $ _mode config == COMPUTER && _player game == 2

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
isGameOver = gets (all ((== 0) . snd) . _board) -- same as : all ((== 0) . snd) . _board <$> get

-- Switch player's turn
switchPlayer :: (MonadState Game m) => m ()
switchPlayer = do
    game <- get
    put $ game { _player = if _player game == 1 then 2 else 1 }

-- Try to convert user's input into a well-formed move
parsePlayerMove :: String -> Maybe Move
parsePlayerMove s
    | length (filter (== ' ') s) /= 1 = Nothing
    | otherwise                       = do
        let w = words s
        r <- readMaybe $ head w :: Maybe RowNo
        n <- readMaybe $ last w :: Maybe NbSticks
        Just (r, n)

{----------------------------------------------------
Rendering Functions
-----------------------------------------------------}

-- Display board
renderBoard :: (MonadReader Config m, MonadState Game m, MonadIO m) => m ()
renderBoard = do
    config <- ask
    game   <- get

    liftIO $ do
        mapM_ renderRow ((,) <$> _board game <*> [_symbol config])
        displayHorizontalLine
        putStrLn ""

renderRow :: (Row, StickSymbol) -> IO ()
renderRow ((r, m), s) = do
    displayHorizontalLine
    putStr $ "| " ++ show r ++ " ==> "++ printf "%02d" m ++ " | "
    putStrLn $ intersperse ' ' (replicate m s)

displayHorizontalLine :: IO ()
displayHorizontalLine = putStrLn $ replicate cHLineLength '-'


{----------------------------------------------------
User interaction Functions
-----------------------------------------------------}

-- Capture user's move
getPlayerMove :: (MonadState Game m, MonadIO m) => m (Maybe Move)
getPlayerMove = do
    game <- get

    liftIO $ putStrLn $ "Player " ++ show (_player game) ++ ", Enter your move (row number <whitespace> number of sticks to remove) :"
    input <- liftIO getLine

    return $ parsePlayerMove input

{----------------------------------------------------
Notification Functions
-----------------------------------------------------}

notifyInvalidMove :: IO ()
notifyInvalidMove = putStrLn "\nInvalid input for move ! Please try again."

notifyIncorrectMove :: IO ()
notifyIncorrectMove = putStrLn "\nIncorrect move: move cannot be played in regard to the board game ! Please try again."

notifyPlayerMove :: (MonadState Game m, MonadIO m) => Move -> m ()
notifyPlayerMove move = do
    game <- get
    let
        player = _player game
        ro     = fst move
        st     = snd move
    liftIO $ putStrLn $ "\nPlayer " ++ show player ++ " removed " ++ show st ++ " stick(s) from row " ++ show ro

notifyWinner :: (MonadState Game m, MonadIO m) => m ()
notifyWinner = do
    game <- get
    liftIO $ putStrLn $ "\nCongratulations! Player " ++ show (_player game) ++ " - You won !!"

{----------------------------------------------------
Logging Functions
-----------------------------------------------------}

logPlayerMove :: (MonadState Game m, MonadWriter String m) => Move -> m ()
logPlayerMove move = do
    game <- get
    let
        board  = _board game
        player = _player game
        ro     = fst move
        st     = snd move
    tell $ "Board: " ++ show board ++ " - Move: (" ++ show ro ++ ", " ++ show st ++ ") -> Player " ++ show player ++ " removed " ++ show st ++ " stick(s) from row " ++ show ro ++ "\n"

{----------------------------------------------------
Main Functions
-----------------------------------------------------}

-- Use of ReaderT to read configuration stored in Config
-- Use of StateT to manage the state of the game stored in Game
-- Use of WriterT to log history of players' moves
playGame :: ReaderT Config (StateT Game (WriterT String IO)) ()
playGame = do
    config <- ask
    game   <- get

    -- Display game board
    renderBoard

    -- Who's turn? Computer or Player?
    computerPlays <- isComputerTurn
    if not computerPlays
        then do -- Player's turn
            -- Get player's move from input
            mm <- getPlayerMove
            -- Clear screen
            liftIO clearScreen
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
                            when continue playGame
        else do -- Computer's turn
            -- Process computer's move and continue if game is not over
            let nextMove = playNextMove $ _board game
            continue <- processMove nextMove
            when continue playGame

-- Process move and return True if game is to be continued
processMove :: (MonadState Game m, MonadWriter String m, MonadIO m) => Move -> m Bool
processMove move = do
    game <- get

    -- Notify move
    notifyPlayerMove move

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
            return False -- Game is over
        else do
            switchPlayer
            return True  -- Game is to be continued

-- Change state in regard to player's move
playMove :: (MonadState Game m) => Move -> m ()
playMove move = do
    game <- get

    -- Register player's move
    put $ game {_move = move}

    -- Update board to reflect the move
    modify applyMove

