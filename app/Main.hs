

module Main where


import Crowdfunding.Trace (simulateCrowdfunding)
import Game.Trace         (simulateGame        )
import Notary.Trace       (simulateNotary      )
import Simple.Trace       (simulateSimple      )


main :: IO ()
main =
  do
    simulateSimple
    simulateGame
    simulateCrowdfunding
    simulateNotary
