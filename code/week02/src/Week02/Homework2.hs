{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map as Map
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Plutus.Contract hiding (when)
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Ledger hiding (Singleton)
import Ledger.Constraints as Constraints
import qualified Ledger.Scripts as Scripts -- needed by validatorHash, etc
import qualified Ledger.Typed.Scripts as Scripts -- needed by Typed
import Ledger.Ada as Ada
import Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Prelude (Semigroup (..))
import Text.Printf (printf)

data RedeemerBools = RedeemerBools
    { flag1 :: Bool
    , flag2 :: Bool
    } deriving (Generic, FromJSON, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''RedeemerBools

{-# INLINABLE mkValidator #-}
mkValidator :: () -> RedeemerBools -> ValidatorCtx -> Bool
mkValidator () (RedeemerBools { flag1, flag2 }) _ = traceIfFalse "flag1 is not equal to flag2" $ flag1 == flag2

data Typed
instance Scripts.ScriptType Typed where
    type instance DatumType Typed = ()
    type instance RedeemerType Typed = RedeemerBools

inst :: Scripts.ScriptInstance Typed
inst = Scripts.validator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @RedeemerBools

validator :: Validator
validator = Scripts.validatorScript inst

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = ScriptAddress valHash

type GiftSchema =
    BlockchainActions
        .\/ Endpoint "give" Integer
        .\/ Endpoint "grab" RedeemerBools

give :: (HasBlockchainActions s, AsContractError e) => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTxConstraints inst tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => RedeemerBools -> Contract w s e ()
grab rb = do
    utxos <- utxoAt scrAddress
    let orefs = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData rb | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = (give' `select` grab') >> endpoints
    where
        give' = endpoint @"give" >>= give
        grab' = endpoint @"grab" >>= grab

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []
