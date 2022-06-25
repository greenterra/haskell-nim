module Configuration where

{----------------------------------------------------
Importing modules
-----------------------------------------------------}

import Text.Read            hiding ( lift, get )

-- local modules
import Model


{----------------------------------------------------
Set configuration from IO
-----------------------------------------------------}

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
    putStrLn $ "Which symbol to use to show as a stick? Enter a single character or press [enter] for default ( = " ++ [cSymbol] ++ " )"
    input <- getLine
    if input == ""
        then return cSymbol -- default value defined in constants section
        -- else
        else if length input == 1
            then return $ head input
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

