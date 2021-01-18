{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}


module Simple.Trace (
  simulateSimple
, publishTrace
, redeemTrace
) where


import Simple
import Language.Plutus.Contract.Trace (ContractTrace)
import Trace (simulate)

import qualified Language.Plutus.Contract.Trace as Trace
import qualified Ledger.Ada                     as Ada


publishTrace :: ContractTrace SimpleSchema e () ()
publishTrace =
  let
    w1 = Trace.Wallet 1
  in
    Trace.callEndpoint @"publish" w1 (PublishDatum 1, Ada.lovelaceValueOf 10)
      >> Trace.handleBlockchainEvents w1
      >> Trace.addBlocks 1


redeemTrace :: ContractTrace SimpleSchema e () ()
redeemTrace =
  let
    w2 = Trace.Wallet 2
  in
    publishTrace
      >> Trace.callEndpoint @"redeem" w2 (RedeemDatum 2)
      >> Trace.handleBlockchainEvents w2
      >> Trace.addBlocks 1


expiredTrace :: ContractTrace SimpleSchema e () ()
expiredTrace =
  let
    w2 = Trace.Wallet 2
  in
    publishTrace
      >> Trace.addBlocks 5
      >> Trace.callEndpoint @"redeem" w2 (RedeemDatum 2)
      >> Trace.handleBlockchainEvents w2
      >> Trace.addBlocks 1


simulateSimple :: IO ()
simulateSimple =
  do
    putStrLn ""
    putStrLn "===== Simple ====="
    putStrLn ""
    simulate simpleAddress simple
      [
        ("publish", publishTrace)
      , ("redeem" , redeemTrace )
      , ("expired", expiredTrace)
      ]
