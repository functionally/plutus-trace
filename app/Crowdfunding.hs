{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


module Crowdfunding (
  Campaign(..)
, CrowdfundingSchema
, crowdfunding
, theCampaign
, contribute
, scheduleCollection
, campaignAddress
, contributionScript
, mkValidator
, mkCampaign
, CampaignAction(..)
, collectionRange
, refundRange
, Contribution(..)
) where




import Control.Applicative (Applicative (..))
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Plutus.Contract
import Language.PlutusTx.Prelude hiding (Applicative (..), Semigroup(..), return, (<$>), (>>), (>>=))
import Ledger (PubKeyHash, Slot, Validator, txId)
import Ledger.Slot (SlotRange)
import Ledger.Validation as V
import Ledger.Value (Value)
import Prelude (Semigroup(..))
import Wallet.Emulator (Wallet)

import qualified Data.Text as Text
import qualified Language.Plutus.Contract.Typed.Tx as Typed
import qualified Language.PlutusTx                 as PlutusTx
import qualified Ledger                            as Ledger
import qualified Ledger.Ada                        as Ada
import qualified Ledger.Constraints                as Constraints
import qualified Ledger.Interval                   as Interval
import qualified Ledger.Scripts                    as Scripts
import qualified Ledger.Typed.Scripts              as Scripts
import qualified Ledger.Value                      as Value
import qualified Prelude                           as Haskell
import qualified Wallet.Emulator                   as Emulator


-- | A crowdfunding campaign.
data Campaign = Campaign
    { campaignDeadline           :: Slot
    -- ^ The date by which the campaign target has to be met
    , campaignTarget             :: Value
    -- ^ Target amount of funds
    , campaignCollectionDeadline :: Slot
    -- ^ The date by which the campaign owner has to collect the funds
    , campaignOwner              :: PubKeyHash
    -- ^ Public key of the campaign owner. This key is entitled to retrieve the
    --   funds if the campaign is successful.
    } deriving (Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''Campaign


-- | Action that can be taken by the participants in this contract. A value of
--   `CampaignAction` is provided as the redeemer. The validator script then
--   checks if the conditions for performing this action are met.
--
data CampaignAction = Collect | Refund

PlutusTx.makeIsData ''CampaignAction
PlutusTx.makeLift ''CampaignAction


type CrowdfundingSchema =
    BlockchainActions
        .\/ Endpoint "schedule collection" ()
        .\/ Endpoint "contribute" Contribution

newtype Contribution = Contribution
        { contribValue :: Value
        -- ^ how much to contribute
        } deriving stock (Haskell.Eq, Show, Generic)
          deriving anyclass (ToJSON, FromJSON)


-- | Construct a 'Campaign' value from the campaign parameters,
--   using the wallet's public key.
mkCampaign :: Slot -> Value -> Slot -> Wallet -> Campaign
mkCampaign ddl target collectionDdl ownerWallet =
    Campaign
        { campaignDeadline = ddl
        , campaignTarget   = target
        , campaignCollectionDeadline = collectionDdl
        , campaignOwner = pubKeyHash $ Emulator.walletPubKey ownerWallet
        }


-- | The 'SlotRange' during which the funds can be collected
collectionRange :: Campaign -> SlotRange
collectionRange cmp =
    Interval.interval (campaignDeadline cmp) (campaignCollectionDeadline cmp)


-- | The 'SlotRange' during which a refund may be claimed
refundRange :: Campaign -> SlotRange
refundRange cmp =
    Interval.from (campaignCollectionDeadline cmp)


data Crowdfunding
instance Scripts.ScriptType Crowdfunding where
    type instance RedeemerType Crowdfunding = CampaignAction
    type instance DatumType Crowdfunding = PubKeyHash


scriptInstance :: Campaign -> Scripts.ScriptInstance Crowdfunding
scriptInstance cmp = Scripts.validator @Crowdfunding
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode cmp)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @PubKeyHash @CampaignAction


{-# INLINABLE validRefund #-}
validRefund :: Campaign -> PubKeyHash -> TxInfo -> Bool
validRefund campaign contributor txinfo =
    -- Check that the transaction falls in the refund range of the campaign
    Interval.contains (refundRange campaign) (txInfoValidRange txinfo)
    -- Check that the transaction is signed by the contributor
    && (txinfo `V.txSignedBy` contributor)


{-# INLINABLE validCollection #-}
validCollection :: Campaign -> TxInfo -> Bool
validCollection campaign txinfo =
    -- Check that the transaction falls in the collection range of the campaign
    (collectionRange campaign `Interval.contains` txInfoValidRange txinfo)
    -- Check that the transaction is trying to spend more money than the campaign
    -- target (and hence the target was reached)
    && (valueSpent txinfo `Value.geq` campaignTarget campaign)
    -- Check that the transaction is signed by the campaign owner
    && (txinfo `V.txSignedBy` campaignOwner campaign)


{-# INLINABLE mkValidator #-}
-- | The validator script is of type 'CrowdfundingValidator', and is
-- additionally parameterized by a 'Campaign' definition. This argument is
-- provided by the Plutus client, using 'PlutusTx.applyCode'.
-- As a result, the 'Campaign' definition is part of the script address,
-- and different campaigns have different addresses. The Campaign{..} syntax
-- means that all fields of the 'Campaign' value are in scope
-- (for example 'campaignDeadline' in l. 70).
mkValidator :: Campaign -> PubKeyHash -> CampaignAction -> ValidatorCtx -> Bool
mkValidator c con act p = case act of
    -- the "refund" branch
    Refund -> validRefund c con (valCtxTxInfo p)
    -- the "collection" branch
    Collect -> validCollection c (valCtxTxInfo p)


-- | The validator script that determines whether the campaign owner can
--   retrieve the funds or the contributors can claim a refund.
contributionScript :: Campaign -> Validator
contributionScript = Scripts.validatorScript . scriptInstance


-- | The address of a [[Campaign]]
campaignAddress :: Campaign -> Ledger.Address
campaignAddress = Ledger.scriptAddress . contributionScript


-- | The crowdfunding contract for the 'Campaign'.
crowdfunding :: Campaign -> Contract CrowdfundingSchema ContractError ()
crowdfunding c = contribute c `select` scheduleCollection c


-- | A sample campaign with a target of 20 Ada by slot 20
theCampaign :: Campaign
theCampaign = Campaign
    { campaignDeadline = 20
    , campaignTarget   = Ada.lovelaceValueOf 20
    , campaignCollectionDeadline = 30
    , campaignOwner = pubKeyHash $ Emulator.walletPubKey (Emulator.Wallet 1)
    }


-- | The "contribute" branch of the contract for a specific 'Campaign'. Exposes
--   an endpoint that allows the user to enter their public key and the
--   contribution. Then waits until the campaign is over, and collects the
--   refund if the funding target was not met.
contribute :: Campaign -> Contract CrowdfundingSchema ContractError ()
contribute cmp = do
    Contribution{contribValue} <- endpoint @"contribute"
    logInfo @Text $ "Contributing " <> Text.pack (show contribValue)
    contributor <- ownPubKey
    let inst = scriptInstance cmp
        tx = Constraints.mustPayToTheScript (pubKeyHash contributor) contribValue
                <> Constraints.mustValidateIn (Ledger.interval 1 (campaignDeadline cmp))
    txid <- fmap txId (submitTxConstraints inst tx)

    utxo <- watchAddressUntil (Scripts.scriptAddress inst) (campaignCollectionDeadline cmp)

    -- 'utxo' is the set of unspent outputs at the campaign address at the
    -- collection deadline. If 'utxo' still contains our own contribution
    -- then we can claim a refund.

    let flt Ledger.TxOutRef{txOutRefId} _ = txid Haskell.== txOutRefId
        tx' = Typed.collectFromScriptFilter flt utxo Refund
                <> Constraints.mustValidateIn (refundRange cmp)
                <> Constraints.mustBeSignedBy (pubKeyHash contributor)
    if Constraints.modifiesUtxoSet tx'
    then do
        logInfo @Text "Claiming refund"
        void (submitTxConstraintsSpending inst utxo tx')
    else pure ()


-- | The campaign owner's branch of the contract for a given 'Campaign'. It
--   watches the campaign address for contributions and collects them if
--   the funding goal was reached in time.
scheduleCollection :: Campaign -> Contract CrowdfundingSchema ContractError ()
scheduleCollection cmp = do
    let inst = scriptInstance cmp

    -- Expose an endpoint that lets the user fire the starting gun on the
    -- campaign. (This endpoint isn't technically necessary, we could just
    -- run the 'trg' action right away)
    () <- endpoint @"schedule collection"
    logInfo @Text "Campaign started. Waiting for campaign deadline to collect funds."

    _ <- awaitSlot (campaignDeadline cmp)
    unspentOutputs <- utxoAt (Scripts.scriptAddress inst)

    let tx = Typed.collectFromScript unspentOutputs Collect
            <> Constraints.mustValidateIn (collectionRange cmp)

    logInfo @Text "Collecting funds"
    void $ submitTxConstraintsSpending inst unspentOutputs tx
