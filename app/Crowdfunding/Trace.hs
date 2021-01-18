-- | Crowdfunding contract implemented using the [[Plutus]] interface.
-- This is the fully parallel version that collects all contributions
-- in a single transaction. This is, of course, limited by the maximum
-- number of inputs a transaction can have.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS -fplugin-opt Language.PlutusTx.Plugin:debug-context #-}

module Crowdfunding.Trace (
    -- * Traces
      startCampaign
    , makeContribution
    , successfulTrace
    , simulateCrowdfunding
    ) where

import           Control.Applicative               (Applicative (..))
import           Control.Monad                     (void)
import           Data.Aeson                        (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Generics                      (Generic)

import           Language.Plutus.Contract
import           Language.Plutus.Contract.Trace    (ContractTrace)
import qualified Language.Plutus.Contract.Trace    as Trace
import qualified Language.Plutus.Contract.Typed.Tx as Typed
import qualified Language.PlutusTx                 as PlutusTx
import           Language.PlutusTx.Prelude         hiding (Applicative (..), Semigroup(..), return, (<$>), (>>), (>>=))
import           Ledger                            (PubKeyHash, Slot, Validator, txId)
import qualified Ledger                            as Ledger
import qualified Ledger.Ada                        as Ada
import qualified Ledger.Constraints                as Constraints
import qualified Ledger.Interval                   as Interval
import           Ledger.Slot                       (SlotRange)
import qualified Ledger.Typed.Scripts              as Scripts
import qualified Ledger.Scripts                    as Scripts
import           Ledger.Validation                 as V
import           Ledger.Value                      (Value)
import qualified Ledger.Value                      as Value
import qualified Prelude                           as Haskell
import           Prelude                           (Semigroup(..))
import           Wallet.Emulator                   (Wallet)
import qualified Wallet.Emulator                   as Emulator
import Crowdfunding
import Trace (simulate)

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
successfulTrace
    :: ContractTrace CrowdfundingSchema ContractError () ()
successfulTrace =
    startCampaign
        >> makeContribution (Trace.Wallet 2) (Ada.lovelaceValueOf 10)
        >> makeContribution (Trace.Wallet 3) (Ada.lovelaceValueOf 10)
        >> makeContribution (Trace.Wallet 4) (Ada.lovelaceValueOf 1)
        >> Trace.addBlocksUntil 20
        >> Trace.handleBlockchainEvents (Trace.Wallet 1)
        >> Trace.addBlocks 1


simulateCrowdfunding :: IO ()
simulateCrowdfunding =
  do
    putStrLn ""
    putStrLn "===== Crowdfunding ====="
    putStrLn ""
    simulate (campaignAddress theCampaign) (crowdfunding theCampaign)
      [
        ("successful", successfulTrace)
      ]
