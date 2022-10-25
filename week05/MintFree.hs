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

module Week05.Free where

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
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy () _ = True                    -- this is the simplest minting policy, where it always returns True

policy :: Scripts.MintingPolicy
policy = mkMinitngPolicyScript $$(PlutusTx.complie [|| Script.wrapMintingPolicy mkPolicy ||])

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy

data MintParams = MintParams
    { mpTokenName   :: !TokenName
    , mpAmount      :: !Integer
    }

type FreeSchema = Endpoint "mint" MintParams -- One endpont called "mint"

mint :: MintParams -> Contract w FreeSchema Text () -- mint takes TokenName and Number - if it is positive it will mnt, if it is negative it will burn
mint mp = do
    let val     = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp)  -- first we get the value to mint
        lookups = Constraints.mintingPolicy policy                          -- we check the minting policy
        tx      = Constraints.mustMintValue val                             -- must mint Value specified here
    ledgerTx <- submitTxConstraintsWith @Void lookups tx                    -- when we submit it will automatically take ffees ffrom wallet, if it is burning it will check for the amount of tokens in users wallet to burn
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () FreeSchema Text ()         -- we define the endpoint, call mint and recursively call it again
endpoints = mint' >> endpoints                      -- mint' is using mint with the endpoint function to expose the endpoint
  where
    mint' = awaitPromise $ endpoint @"mint" mint

mkSchemaDefinitions ''FreeSchema

mkKnownCurrencies []

test :: IO ()                                               -- this is an emulator trace so we can run it in the repl
test = runEmulatorTraceIO $ do                              -- this is just like last week when working with EmTrace
    let tn = "ABC"                                          -- this is the timeline of the test
    h1 <- activateContractWallet (knownWallet 1) endpoints    
    h2 <- activateContractWallet (knownWallet 2) endpoints
    callEndpoint @"mint" h1 $ MintParams                    -- Wallet 1 mints 555
        { mpTokenName = tn
        , mpAmount    = 555
        }
    callEndpoint @"mint" h2 $ MintParams                    -- Wallet 2 mints 444
        { mpTokenName = tn
        , mpAmount    = 444
        }
    void $ Emulator.waitNSlots 1                            -- Wallet 1 BURNS 222
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = -222
        }
    void $ Emulator.waitNSlots 1


-- When you run test in the Repl
{-
Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58: 
    {, ""}: 99997467
    {983cf28fd53484a220dc42c0ae430b0c1c16b210a72975c5a98dec63, "ABC"}: 444
Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491: 
    {983cf28fd53484a220dc42c0ae430b0c1c16b210a72975c5a98dec63, "ABC"}: 333
    {, ""}: 99994934

-}    
