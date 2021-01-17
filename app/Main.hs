

module Main where


import Game.Trace (simulateGame)
import Simple.Trace (simulateSimple)


main :: IO ()
main =
  do
--  simulateGame
    simulateSimple
