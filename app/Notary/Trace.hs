{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}


module Notary.Trace (
  simulateNotary
) where


import Notary
import Language.Plutus.Contract.Trace (ContractTrace)
import Trace (simulate)
import Wallet.Emulator (Wallet(Wallet), walletPubKey)
import Ledger (pubKeyHash)

import qualified Language.Plutus.Contract.Trace as Trace
import qualified Ledger.Ada                     as Ada

import qualified Data.ByteString.Char8 as C

import Language.PlutusTx.Prelude (sha3_256)


simpleTrace :: ContractTrace NotarySchema e () ()
simpleTrace =
  do
    let
      w0 = Trace.Wallet 10
      w1 = Trace.Wallet 1
      w2 = Trace.Wallet 2
      w3 = Trace.Wallet 3
      w4 = Trace.Wallet 4
      n1 = Notarization {hash = sha3_256 $ C.pack "test 1", description = C.pack "test 1"}
      n2 = Notarization {hash = sha3_256 $ C.pack "test 2", description = C.pack "test 2"}
      n3 = Notarization {hash = sha3_256 $ C.pack "test 3", description = C.pack "test 3"}
      n4 = Notarization {hash = sha3_256 $ C.pack "test 4", description = C.pack "test 4"}
    Trace.callEndpoint @"notarize" w1 (n1, Ada.lovelaceValueOf 10)
    Trace.handleBlockchainEvents w1
    Trace.addBlocks 1
    Trace.callEndpoint @"notarize" w2 (n2, Ada.lovelaceValueOf 20)
    Trace.handleBlockchainEvents w2
    Trace.addBlocks 1
    Trace.callEndpoint @"publish" w0 ()
    Trace.handleBlockchainEvents w0
    Trace.addBlocks 1
    Trace.callEndpoint @"notarize" w3 (n3, Ada.lovelaceValueOf 30)
    Trace.handleBlockchainEvents w3
    Trace.callEndpoint @"notarize" w4 (n4, Ada.lovelaceValueOf 40)
    Trace.handleBlockchainEvents w4
    Trace.addBlocksUntil 12
    Trace.handleBlockchainEvents w0
    Trace.addBlocks 1


simulateNotary :: IO ()
simulateNotary =
  do
    let
      service = Service . pubKeyHash . walletPubKey $ Wallet 10
    putStrLn ""
    putStrLn "===== Notary ====="
    putStrLn ""
    simulate (notaryAddress service) (notary service)
      [
        ("simple", simpleTrace)
      ]
