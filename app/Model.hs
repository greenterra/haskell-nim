module Model where

{----------------------------------------------------
Importing modules
-----------------------------------------------------}


{----------------------------------------------------
Data Model
-----------------------------------------------------}

type NbRows      = Int   -- nb of rows
type ListSticks  = [Int] -- configuration of rows
type StickSymbol = Char  -- character symbol representing a stick to be shown when displaying the game board

data PlayMode    = COMPUTER | HUMAN deriving (Eq, Show)

-- Reader Config

data Config = Config {          -- Game configuration
      _nbRows     :: NbRows
    , _listSticks :: ListSticks
    , _symbol     :: StickSymbol
    , _mode       :: PlayMode
    } deriving (Show)

----------------------------------------------------

type RowNo    = Int               -- row number
type NbSticks = Int               -- quantity of sticks

type Row      = (RowNo, NbSticks) -- a row is a tuple (row number starting from 1, quantity of sticks left in that row)
type Board    = [Row]             -- list of rows
type Player   = Int               -- current player : 1 or 2
type Move     = (RowNo, NbSticks) -- a move is a tuple (row number, quantity of sticks to remove)

-- State Game

data Game = Game {
      _board  :: Board
    , _player :: Player
    , _move   :: Move
    } deriving (Show)

{----------------------------------------------------
Constants
-----------------------------------------------------}

cNbRows :: NbRows
cNbRows      = 3            -- default nb of rows being proposed when asked for desired configuration
cListSticks :: ListSticks
cListSticks  = [9, 16, 13]  -- default composition of rows being proposed when asked for desired configuration
cSymbol :: StickSymbol
cSymbol      = '!'          -- default character symbol being proposed when asked for desired configuration
cHLineLength :: Int
cHLineLength = 50           -- length of horizontal line separating rows when rendering game board

