
module Direction (
Direction(N, E, S, W), Position
) where

data Direction = N | E | S | W
        deriving (Show)

type Position = (Int, Int)
