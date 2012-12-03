module Tennis
( Player(..)
, Game
, newGame
, score
) where

data Player = A
            | B

data Score = Love
           | Fifteen
           | Thirty
           | Forty
           deriving (Enum)

data Game = Game Score Score
          | Deuce
          | AdvantageA
          | AdvantageB
          | WinnerA
          | WinnerB

instance Show Game where
    show AdvantageA = "Advantage A"
    show AdvantageB = "Advantage B"
    show Deuce      = "Deuce"
    show (Game x y) = show x ++ " - " ++ show y
    show WinnerA    = "A Wins"
    show WinnerB    = "B Wins"

instance Show Score where
    show Love    = "Love"
    show Fifteen = "15"
    show Thirty  = "30"
    show Forty   = "40"

newGame :: Game
newGame = (Game Love Love)

score :: Player -> Game -> Game
score A AdvantageA          = WinnerA
score B AdvantageB          = WinnerB
score A AdvantageB          = Deuce
score B AdvantageA          = Deuce
score A Deuce               = AdvantageA
score B Deuce               = AdvantageB
score A (Game Thirty Forty) = Deuce
score B (Game Forty Thirty) = Deuce
score A (Game Forty _)      = WinnerA
score B (Game _ Forty)      = WinnerB
score A (Game x y)          = Game (succ x) y
score B (Game x y)          = Game w z
  where
    (Game z w) = score A (Game y x)
score _ gameFinished        = gameFinished
