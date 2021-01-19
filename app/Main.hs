

module Main where


import Crowdfunding.Trace (simulateCrowdfunding)
import Game.Trace         (simulateGame        )
import Simple.Trace       (simulateSimple      )


main :: IO ()
main =
  do
    simulateSimple
    simulateGame
    simulateCrowdfunding
