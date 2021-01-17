{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}


module Game.Trace (
  simulateGame
, lockTrace
, guessRightTrace
, guessWrongTrace
) where


import Game
import Language.Plutus.Contract.Trace (ContractTrace)
import Trace (simulate)

import qualified Language.Plutus.Contract.Trace as Trace
import qualified Ledger.Ada                     as Ada


lockTrace :: ContractTrace GameSchema e () ()
lockTrace =
  let
    w1 = Trace.Wallet 1
  in
    Trace.callEndpoint @"lock" w1 (LockParams "secret" (Ada.lovelaceValueOf 10))
      >> Trace.handleBlockchainEvents w1
      >> Trace.addBlocks 1


guessRightTrace :: ContractTrace GameSchema e () ()
guessRightTrace =
  let
    w2 = Trace.Wallet 2
  in
    lockTrace
      >> Trace.callEndpoint @"guess" w2 (GuessParams "secret")
      >> Trace.handleBlockchainEvents w2
      >> Trace.addBlocks 1


guessWrongTrace :: ContractTrace GameSchema e () ()
guessWrongTrace =
  let
    w2 = Trace.Wallet 2
  in
    lockTrace
      >> Trace.callEndpoint @"guess" w2 (GuessParams "SECRET")
      >> Trace.handleBlockchainEvents w2
      >> Trace.addBlocks 1


simulateGame :: IO ()
simulateGame =
  do
    putStrLn ""
    putStrLn "===== Game ====="
    putStrLn ""
    simulate gameAddress game
      [
        ("Lock"       , lockTrace      )
      , ("Guess Right", guessRightTrace)
      , ("Guess Wrong", guessWrongTrace)
      ]
