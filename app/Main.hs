{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}


module Main where


import Control.Monad.Freer (Eff)
import Data.Map (Map, (!), toList)
import Game
import Language.Plutus.Contract (Contract, ContractError)
import Language.Plutus.Contract.Resumable (Response(Response))
import Language.Plutus.Contract.Schema (Event)
import Language.Plutus.Contract.Test (fundsAtAddress)
import Language.Plutus.Contract.Trace (ContractTrace, ContractTraceEffs, ContractTraceResult(ContractTraceResult), ContractTraceState, TraceError, eventsByWallet, runTrace)
import Data.Text.Prettyprint.Doc (pretty)
import Wallet.Emulator (EmulatorState, Wallet)
import Wallet.Emulator.MultiAgent (fundsDistribution)

import qualified Language.Plutus.Contract.Trace as Trace
import qualified Ledger.Ada                     as Ada
import qualified Wallet.Emulator                as EM
import qualified Wallet.Emulator.Chain          as EM
import qualified Wallet.Emulator.MultiAgent     as EM
import qualified Wallet.Emulator.NodeClient     as EM


lockTrace :: ContractTrace GameSchema e () ()
lockTrace =
  let
    w1 = Trace.Wallet 1
  in
    Trace.callEndpoint @"lock" w1 (LockParams "secret" (Ada.lovelaceValueOf 10))
      >> Trace.handleBlockchainEvents w1
      >> Trace.addBlocks 1


guessTrace :: ContractTrace GameSchema e () ()
guessTrace =
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


doTrace :: Contract GameSchema ContractError a
        -> Eff (ContractTraceEffs GameSchema ContractError a) ()
        -> (Either (TraceError ContractError) (ContractTraceState GameSchema (TraceError ContractError) a), EmulatorState)
doTrace contract effect =
  let
    (result, emulatorState) = runTrace contract effect
  in
    case result of
      Right ((), traceState) -> (Right traceState, emulatorState)
      Left  e                -> (Left e          , emulatorState)


printEvents :: EmulatorState -> Map Wallet [Response (Event GameSchema)] -> IO ()
printEvents state events =
  do
    putStrLn ""
    print gameAddress
    sequence_
      [
        do
          putStrLn ""
          print wallet
          print $ pretty responses
          print $ fundsDistribution state ! wallet
      |
        (wallet, responses) <- toList events
      , not $ null responses
      ]
    putStrLn ""
    print $ pretty $ EM._emulatorLog state


main :: IO ()
main =
  sequence_
    [
      do
        putStrLn ""
        putStrLn $ "----- " ++ name ++ " -----"
        let
          (traceState, emulatorState) = doTrace game effect
        either print (printEvents emulatorState . eventsByWallet) traceState
        putStrLn ""
    |
      (name, effect) <- [
                          ("Lock"       , lockTrace      )
                        , ("Guess"      , guessTrace     )
                        , ("Guess Wrong", guessWrongTrace)
                        ]
    ]
