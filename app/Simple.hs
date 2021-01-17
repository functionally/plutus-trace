{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module Simple where


import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Language.Plutus.Contract
import Language.Plutus.Contract (AsContractError, BlockchainActions, Contract, Endpoint, type (.\/), collectFromScript, endpoint, select, submitTxConstraints, submitTxConstraintsSpending, submitTxConstraintsSpending, utxoAt)
import Language.PlutusTx (Data(I))
import Language.PlutusTx.Prelude  hiding (Applicative (..))
import Ledger (Address, ValidatorCtx, scriptAddress)
import Ledger.Value (Value)

import qualified Language.PlutusTx    as PlutusTx
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import qualified Prelude


newtype PublishDatum = PublishDatum Integer
  deriving stock (Prelude.Eq, Prelude.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance PlutusTx.IsData PublishDatum where
  toData (PublishDatum i) = I i
  fromData (I i) = Just $ PublishDatum i
  fromData _     = Nothing

PlutusTx.makeLift ''PublishDatum


data RedeemDatum = RedeemDatum Integer
  deriving stock (Prelude.Eq, Prelude.Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance PlutusTx.IsData RedeemDatum where
  toData (RedeemDatum i) = I i
  fromData (I i) = Just $ RedeemDatum i
  fromData _     = Nothing

PlutusTx.makeLift ''RedeemDatum


validateSpend :: PublishDatum -> RedeemDatum -> ValidatorCtx -> Bool
validateSpend _ _ _ = True


simpleAddress :: Address
simpleAddress = Ledger.scriptAddress $ Scripts.validatorScript simpleInstance


data Simple

instance Scripts.ScriptType Simple where
  type instance RedeemerType Simple = RedeemDatum
  type instance DatumType    Simple = PublishDatum


simpleInstance :: Scripts.ScriptInstance Simple
simpleInstance =
  Scripts.validator @Simple
    $$(PlutusTx.compile [|| validateSpend ||])
    $$(PlutusTx.compile [|| wrap          ||])
      where
        wrap = Scripts.wrapValidator @PublishDatum @RedeemDatum


type SimpleSchema =
  BlockchainActions
    .\/ Endpoint "publish" (PublishDatum, Value)
    .\/ Endpoint "redeem" RedeemDatum


simple :: AsContractError e => Contract SimpleSchema e ()
simple = publish `select` redeem


publish :: AsContractError e => Contract SimpleSchema e ()
publish =
  do
    (PublishDatum i, lockedFunds) <- endpoint @"publish" @(PublishDatum, Value)
    let
      tx = Constraints.mustPayToTheScript (PublishDatum i) lockedFunds
    void $ submitTxConstraints simpleInstance tx


redeem :: AsContractError e => Contract SimpleSchema e ()
redeem =
  do
    RedeemDatum myRedeemerValue <- endpoint @"redeem" @RedeemDatum
    unspentOutputs <- utxoAt simpleAddress
    let
      redeemer = RedeemDatum myRedeemerValue
      tx       = collectFromScript unspentOutputs redeemer
    void $ submitTxConstraintsSpending simpleInstance unspentOutputs tx
