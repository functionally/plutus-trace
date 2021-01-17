{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}


module Main where


import Control.Monad.Freer (Eff)
import Data.Map (Map, toList)
import Game
import Language.Plutus.Contract (Contract, ContractError)
import Language.Plutus.Contract.Resumable (Response(Response))
import Language.Plutus.Contract.Schema (Event)
import Language.Plutus.Contract.Trace (ContractTrace, ContractTraceEffs, ContractTraceResult(ContractTraceResult), ContractTraceState, TraceError, eventsByWallet, runTrace)
import Data.Text.Prettyprint.Doc (pretty)
import Wallet.Emulator (Wallet)

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
        -> Either (TraceError ContractError) (ContractTraceState GameSchema (TraceError ContractError) a)
doTrace contract effect =
  case fst $ runTrace contract effect of
    Right ((), state) -> Right state
    Left  e           -> Left e


traceEvents :: Contract GameSchema ContractError a
            -> Eff (ContractTraceEffs GameSchema ContractError a) ()
            -> Either (TraceError ContractError) (Map Wallet [Response (Event GameSchema)])
traceEvents contract effect = eventsByWallet <$> doTrace contract effect


printEvents :: Map Wallet [Response (Event GameSchema)] -> IO ()
printEvents events =
  sequence_
    [
      do
        putStrLn ""
        print wallet
        print $ pretty responses
    |
      (wallet, responses) <- toList events
    , not $ null responses
    ]


main :: IO ()
main =
  sequence_
    [
      do
        putStrLn ""
        putStrLn $ "----- " ++ name ++ " -----"
        either print printEvents
          $ traceEvents game effect
        putStrLn ""
    |
      (name, effect) <- [
                          ("Lock"       , lockTrace      )
                        , ("Guess"      , guessTrace     )
                        , ("Guess Wrong", guessWrongTrace)
                        ]
    ]
