{-# LANGUAGE FlexibleContexts #-}


module Trace (
  simulate
) where


import Control.Monad.Freer (Eff)
import Data.Map (Map, (!), toList)
import Data.Text.Prettyprint.Doc (Pretty, pretty)
import Language.Plutus.Contract (Contract, ContractError)
import Language.Plutus.Contract.Resumable (Response(Response))
import Language.Plutus.Contract.Schema (Event, Input, Output)
import Language.Plutus.Contract.Test (fundsAtAddress)
import Language.Plutus.Contract.Trace (ContractTrace, ContractTraceEffs, ContractTraceResult(ContractTraceResult), ContractTraceState, TraceError, eventsByWallet, runTrace)
import Ledger (Address)
import Wallet.Emulator (EmulatorState, Wallet)
import Wallet.Emulator.MultiAgent (fundsDistribution)

import qualified Data.Aeson.Types  as JSON
import qualified Data.Row.Internal as V
import qualified Wallet.Emulator   as EM


doTrace :: V.AllUniqueLabels (Input s)
        => V.Forall (Input s) JSON.FromJSON
        => V.Forall (Output s) V.Unconstrained1
        => Contract s ContractError a
        -> Eff (ContractTraceEffs s ContractError a) ()
        -> (Either (TraceError ContractError) (ContractTraceState s (TraceError ContractError) a), EmulatorState)
doTrace contract effect =
  let
    (result, emulatorState) = runTrace contract effect
  in
    case result of
      Right ((), traceState) -> (Right traceState, emulatorState)
      Left  e                -> (Left e          , emulatorState)


printEvents :: V.AllUniqueLabels (Input s)
            => V.Forall (Input s) JSON.FromJSON
            => V.Forall (Input s) Pretty
            => V.Forall (Output s) V.Unconstrained1
            => Address
            -> EmulatorState
            -> Map Wallet [Response (Event s)]
            -> IO ()
printEvents address state events =
  do
    putStrLn ""
    print address
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


simulate :: V.AllUniqueLabels (Input s)
         => V.Forall (Input s) JSON.FromJSON
         => V.Forall (Input s) Pretty
         => V.Forall (Output s) V.Unconstrained1
         => Address
         -> Contract s ContractError e
         -> [(String, Eff (ContractTraceEffs s ContractError e) ())]
         -> IO ()
simulate address contract effects =
  sequence_
    [
      do
        putStrLn ""
        putStrLn $ "----- " ++ name ++ " -----"
        let
          (traceState, emulatorState) = doTrace contract effect
        either print (printEvents address emulatorState . eventsByWallet) traceState
        putStrLn ""
    |
      (name, effect) <- effects
    ]

