module Tennis
( Player(..)
, Game
, newGame
, score
) where

data Player = A
            | B
            deriving (Show)

data Score = Love
           | Fifteen
           | Thirty
           | Forty
           deriving (Enum)

data Game = Game Score Score
          | Deuce
          | Advantage Player
          | Winner Player

instance Show Game where
    show (Advantage x) = "Advantage " ++ show x
    show Deuce         = "Deuce"
    show (Game x y)    = show x ++ " - " ++ show y
    show (Winner x)    = show x ++ " Wins"

instance Show Score where
    show Love    = "Love"
    show Fifteen = "15"
    show Thirty  = "30"
    show Forty   = "40"

newGame :: Game
newGame = (Game Love Love)

score :: Player -> Game -> Game
score A (Advantage A)       = Winner A
score B (Advantage B)       = Winner B
score A (Advantage _)       = Deuce
score B (Advantage _)       = Deuce
score A Deuce               = Advantage A
score B Deuce               = Advantage B
score A (Game Thirty Forty) = Deuce
score B (Game Forty Thirty) = Deuce
score A (Game Forty _)      = Winner A
score B (Game _ Forty)      = Winner B
score A (Game x y)          = Game (succ x) y
score B (Game x y)          = Game w z
  where
    (Game z w) = score A (Game y x)
score _ gameFinished        = gameFinished
