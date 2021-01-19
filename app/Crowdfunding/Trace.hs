{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}


module Crowdfunding.Trace (
  startCampaign
, makeContribution
, successfulTrace
, simulateCrowdfunding
) where


import Crowdfunding
import Language.Plutus.Contract
import Language.Plutus.Contract.Trace (ContractTrace)
import Language.PlutusTx.Prelude
import Ledger.Value (Value)
import Trace (simulate)
import Wallet.Emulator (Wallet)

import qualified Language.Plutus.Contract.Trace as Trace
import qualified Ledger.Ada                     as Ada


-- | Call the "schedule collection" endpoint and instruct the campaign owner's
--   wallet (wallet 1) to start watching the campaign address.
startCampaign :: ContractTrace CrowdfundingSchema ContractError () ()
startCampaign =
    Trace.callEndpoint @"schedule collection" (Trace.Wallet 1)  ()
        >> Trace.notifyInterestingAddresses (Trace.Wallet 1)


-- | Call the "contribute" endpoint, contributing the amount from the wallet
makeContribution
    :: Wallet
    -> Value
    -> ContractTrace CrowdfundingSchema ContractError () ()
makeContribution w v =
    Trace.callEndpoint @"contribute" w Contribution{contribValue=v}
        >> Trace.handleBlockchainEvents w
        >> Trace.addBlocks 1


-- | Run a successful campaign with contributions from wallets 2, 3 and 4.
successfulTrace :: ContractTrace CrowdfundingSchema ContractError () ()
successfulTrace =
  startCampaign
    >> makeContribution (Trace.Wallet 2) (Ada.lovelaceValueOf 10)
    >> makeContribution (Trace.Wallet 3) (Ada.lovelaceValueOf 10)
    >> makeContribution (Trace.Wallet 4) (Ada.lovelaceValueOf 1)
    >> Trace.addBlocksUntil 20
    >> Trace.handleBlockchainEvents (Trace.Wallet 1)
    >> Trace.addBlocksUntil 30
    >> Trace.handleBlockchainEvents (Trace.Wallet 2)
    >> Trace.handleBlockchainEvents (Trace.Wallet 3)
    >> Trace.handleBlockchainEvents (Trace.Wallet 4)
    >> Trace.addBlocks 1


uncollectedTrace :: ContractTrace CrowdfundingSchema ContractError () ()
uncollectedTrace =
  startCampaign
    >> makeContribution (Trace.Wallet 2) (Ada.lovelaceValueOf 10)
    >> makeContribution (Trace.Wallet 3) (Ada.lovelaceValueOf 10)
    >> makeContribution (Trace.Wallet 4) (Ada.lovelaceValueOf 1)
    >> Trace.addBlocksUntil 40
    >> Trace.handleBlockchainEvents (Trace.Wallet 1)
    >> Trace.handleBlockchainEvents (Trace.Wallet 2)
    >> Trace.handleBlockchainEvents (Trace.Wallet 3)
    >> Trace.handleBlockchainEvents (Trace.Wallet 4)
    >> Trace.addBlocks 1


unfundedTrace :: ContractTrace CrowdfundingSchema ContractError () ()
unfundedTrace =
  startCampaign
    >> makeContribution (Trace.Wallet 2) (Ada.lovelaceValueOf 2)
    >> makeContribution (Trace.Wallet 3) (Ada.lovelaceValueOf 10)
    >> makeContribution (Trace.Wallet 4) (Ada.lovelaceValueOf 1)
    >> Trace.addBlocksUntil 20
    >> Trace.handleBlockchainEvents (Trace.Wallet 1)
    >> Trace.addBlocksUntil 30
    >> Trace.handleBlockchainEvents (Trace.Wallet 2)
    >> Trace.handleBlockchainEvents (Trace.Wallet 3)
    >> Trace.handleBlockchainEvents (Trace.Wallet 4)
    >> Trace.addBlocks 1


lateTrace :: ContractTrace CrowdfundingSchema ContractError () ()
lateTrace =
  startCampaign
    >> makeContribution (Trace.Wallet 2) (Ada.lovelaceValueOf 10)
    >> makeContribution (Trace.Wallet 3) (Ada.lovelaceValueOf 5)
    >> Trace.addBlocksUntil 21
    >> makeContribution (Trace.Wallet 4) (Ada.lovelaceValueOf 6)
    >> Trace.addBlocksUntil 22
    >> Trace.handleBlockchainEvents (Trace.Wallet 1)
    >> Trace.addBlocksUntil 30
    >> Trace.handleBlockchainEvents (Trace.Wallet 2)
    >> Trace.handleBlockchainEvents (Trace.Wallet 3)
    >> Trace.handleBlockchainEvents (Trace.Wallet 4)
    >> Trace.addBlocks 1


multipleTrace :: ContractTrace CrowdfundingSchema ContractError () ()
multipleTrace =
  startCampaign
    >> makeContribution (Trace.Wallet 2) (Ada.lovelaceValueOf 10)
    >> makeContribution (Trace.Wallet 3) (Ada.lovelaceValueOf 5)
    >> makeContribution (Trace.Wallet 4) (Ada.lovelaceValueOf 1)
    >> Trace.addBlocksUntil 10
    >> makeContribution (Trace.Wallet 3) (Ada.lovelaceValueOf 5)
    >> Trace.addBlocksUntil 20
    >> Trace.handleBlockchainEvents (Trace.Wallet 1)
    >> Trace.addBlocksUntil 30
    >> Trace.handleBlockchainEvents (Trace.Wallet 2)
    >> Trace.handleBlockchainEvents (Trace.Wallet 3)
    >> Trace.handleBlockchainEvents (Trace.Wallet 4)
    >> Trace.addBlocks 1


simulateCrowdfunding :: IO ()
simulateCrowdfunding =
  do
    putStrLn ""
    putStrLn "===== Crowdfunding ====="
    putStrLn ""
    simulate (campaignAddress theCampaign) (crowdfunding theCampaign)
      [
        ("Successful"       , successfulTrace )
      , ("Failed to Collect", uncollectedTrace)
      , ("Failed to Fund"   , unfundedTrace   )
      , ("Too Late"         , lateTrace       )
--    , ("Multiple"         , multipleTrace   ) FIXME: Failes with endpoint not active.
      ]
