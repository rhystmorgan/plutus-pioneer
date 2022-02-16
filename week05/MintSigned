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

module Week05.Signed where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

{-# INLINABLE mkPolicy #-}
mkPolicy :: PaymentPubKeyHash -> () -> ScriptContext -> Bool -- this now requires a signature from the Params
mkPolicy pkh () ctx = txSignedBy (scriptContextTxInfo ctx) $ unPaymentPubKeyHash pkh -- we need to get the underlying pubKeyHash remember
                                                                                    -- here we only allow if it was signed by the given pubkeyhash
policy :: PaymentPubKeyHash -> Scripts.MintingPolicy                    -- This is very similar to how the Validator Scripts we did previousy work
policy pkh = mkMintingPolicyScript $                                    -- We are basing the mkValidator on the PubKeyHash
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])   -- For this to work we need to do the 'liftCode' like we did before
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh

curSymbol :: PaymentPubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy           -- this is also based on the policy so we need to add 

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type FreeSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
    pkh <- Contract.ownPaymentPubKeyHash                -- we need a handle for the PubKeyHash
    let val     = Value.singleton (curSymbol pkh) (mpTokenName mp) (mpAmount mp)    -- We apply the PKH to the curSymbol
        lookups = Constraints.mintingPolicy $ policy pkh                            -- we add pkh to the Policy
        tx      = Constraints.mustMintValue val                     
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

mkSchemaDefinitions ''FreeSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 555
        }
    callEndpoint @"mint" h2 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 444
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = -222
        }
    void $ Emulator.waitNSlots 1

-- if you run this in the emulator, the currency symbols will be different - the token name is the same, but currency is different
-- this is different because the CurSymbol is a function, it is based on the pubkeyhash meaning it will be different for each wallet

{-
Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58: 
    {, ""}: 99997273
    {6a23f49d0acb4de48549c11b5f9963861579ae778c65886ab9fbc627, "ABC"}: 444
Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491: 
    {abd8957f184c0b8dae47f4ff1d56c87a3781c15ca6203f7727fa902b, "ABC"}: 333
    {, ""}: 99994546

-}
