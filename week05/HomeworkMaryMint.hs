{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.Homework1 where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Aeson                 (ToJSON, FromJSON)
import           Data.Default               (Default (..))
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Ledger                     hiding (mint, singleton)
import           Ledger.Constraints         as Constraints
import           Ledger.TimeSlot
import qualified Ledger.Typed.Scripts       as Scripts
import           Ledger.Value               as Value
import           Playground.Contract        (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH              (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types           (KnownCurrency (..))
import           Prelude                    (IO, Semigroup (..), Show (..), String, undefined)
import           Text.Printf                (printf)
import           Wallet.Emulator.Wallet

{-# INLINABLE mkPolicy #-}
mkPolicy :: PaymentPubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkPolicy pkh deadline () ctx =  traceIfFalse "Wrong PubKey!" signedByPKH &&
                                traceIfFalse "It's Too Late!" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx 

    signedByPKH :: Bool
    signedByPKH = txSignedBy info $ unPaymentPubKeyHash pkh

    deadlineReached :: Bool
    deadlineReached = contains (to $ deadline) $ txInfoValidRange info
    
    --True -- FIX ME!

policy :: PaymentPubKeyHash -> POSIXTime -> Scripts.MintingPolicy
policy pkh deadline = mkMintingPolicyScript $ 
    $$(PlutusTx.compile [|| \pkh' deadline' -> Scripts.wrapMintingPolicy $ mkPolicy pkh' deadline' ||]) 
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh
    `PlutusTx.applyCode`
    PlutusTx.liftCode deadline
    
    -- undefined -- IMPLEMENT ME!

curSymbol :: PaymentPubKeyHash -> POSIXTime -> CurrencySymbol
curSymbol pkh deadline = scriptCurrencySymbol $ policy pkh deadline
    
    -- undefined -- IMPLEMENT ME!

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpDeadline  :: !POSIXTime
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type SignedSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w SignedSchema Text ()
mint mp = do
    pkh <- Contract.ownPaymentPubKeyHash
    now <- Contract.currentTime
    let deadline = mpDeadline mp
    if now > deadline
        then Contract.logError @String "deadline passed"
        else do
            let val     = Value.singleton (curSymbol pkh deadline) (mpTokenName mp) (mpAmount mp)
                lookups = Constraints.mintingPolicy $ policy pkh deadline
                tx      = Constraints.mustMintValue val <> Constraints.mustValidateIn (to $ now + 60000)
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () SignedSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

mkSchemaDefinitions ''SignedSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn       = "ABC"
        deadline = slotToBeginPOSIXTime def 100
    h <- activateContractWallet (knownWallet 1) endpoints
    callEndpoint @"mint" h $ MintParams
        { mpTokenName = tn
        , mpDeadline  = deadline
        , mpAmount    = 555
        }
    void $ Emulator.waitNSlots 110
    callEndpoint @"mint" h $ MintParams
        { mpTokenName = tn
        , mpDeadline  = deadline
        , mpAmount    = 555
        }
    void $ Emulator.waitNSlots 1


-- Results in the Repl

{-

Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491: 
    {, ""}: 99996196
    {6dd907b201d5c19e17436f74bfbd0c0a3aff4ad9f872aa42c080e6b0, "ABC"}: 555

-}

-- Only 555 Minted because second attempt is past deadline

