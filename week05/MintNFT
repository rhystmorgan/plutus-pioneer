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

module Week05.NFT where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Map               as Map
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
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&   --oref is the output reference
                          traceIfFalse "wrong amount minted" checkMintedAmount      -- this checks the UTXO, and that none have been minted already
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool                                                     -- txInInfoOutRef from TxInfo that consumes the TX Output oref
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info  -- txInfoInputs

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of          -- txInfoMint specifies the token
        [(_, tn', amt)] -> tn' == tn && amt == 1                -- We expect a triple, if anything else it will fail
        _               -> False                                -- We specify that TokenName is expected and Amt amount is 1

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tn = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn' ||])
    `PlutusTx.applyCode`        -- because we have 2 parameters, we need to apply lift twice
    PlutusTx.liftCode oref      -- first with oref
    `PlutusTx.applyCode`        -- seond with the TokenName
    PlutusTx.liftCode tn

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn   -- now we can compute the currencysymbol

data NFTParams = NFTParams
    { npToken   :: !TokenName                   -- NFT Params are what you'd expect
    , npAddress :: !Address                     -- Token Name and User Address
    } deriving (Generic, FromJSON, ToJSON, Show)

type NFTSchema = Endpoint "mint" NFTParams      -- mint Endpint with NFT parameters

mint :: NFTParams -> Contract w NFTSchema Text ()       -- Contract Monad
mint np = do
    utxos <- utxosAt $ npAddress np                     --UTXOAt take second field from param and returns all utxos
    case Map.keys utxos of                              -- maps the UTXO to proide Keys
        []       -> Contract.logError @String "no utxo found"   -- error out
        oref : _ -> do                                  -- if we find ANY then
            let tn      = npToken np                    -- Token Name
            let val     = Value.singleton (curSymbol oref tn) tn 1  -- Value, Curency Symbol, Token name, amount 1
                lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos    -- We don't need all UTXos, it will just look at all to find the one needed
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref           -- Tx must mint, must spend the tx output oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx        -- submit,
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx           -- wait for confirmation
            Contract.logInfo @String $ printf "forged %s" (show val)    -- log message

endpoints :: Contract () NFTSchema Text ()          -- endpoints are the same
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

test :: IO ()                                       -- This is basically the same emulator as before
test = runEmulatorTraceIO $ do
    let tn = "ABC"                                  -- pick Token name
        w1 = knownWallet 1                          -- specify 2 wallets
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints       
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"mint" h1 $ NFTParams             -- call endpoint for first wallet
        { npToken   = tn
        , npAddress = mockWalletAddress w1
        }
    callEndpoint @"mint" h2 $ NFTParams             -- repeat for second wallet
        { npToken   = tn
        , npAddress = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1

    -- Running this will create 2 NFTs with the same name,
    -- each wallet has it's own NFT

    -- Here are the results of the Emulator run in the Repl

{-
    
Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58: 
{, ""}: 99996890
{2d8911eaeda275b8e0c4ba484f1857435a3e5720fa9ac648fc343b57, "ABC"}: 1
Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491: 
{, ""}: 99996890
{dd34121ddddfd43091b6f4368b4ea1715228bfb2e65558942bf052cc, "ABC"}: 1}

-}{-# LANGUAGE DataKinds           #-}
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

module Week05.NFT where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Map               as Map
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
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&   --oref is the output reference
                          traceIfFalse "wrong amount minted" checkMintedAmount      -- this checks the UTXO, and that none have been minted already
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool                                                     -- txInInfoOutRef from TxInfo that consumes the TX Output oref
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info  -- txInfoInputs

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of          -- txInfoMint specifies the token
        [(_, tn', amt)] -> tn' == tn && amt == 1                -- We expect a triple, if anything else it will fail
        _               -> False                                -- We specify that TokenName is expected and Amt amount is 1

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tn = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn' ||])
    `PlutusTx.applyCode`        -- because we have 2 parameters, we need to apply lift twice
    PlutusTx.liftCode oref      -- first with oref
    `PlutusTx.applyCode`        -- seond with the TokenName
    PlutusTx.liftCode tn

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn   -- now we can compute the currencysymbol

data NFTParams = NFTParams
    { npToken   :: !TokenName                   -- NFT Params are what you'd expect
    , npAddress :: !Address                     -- Token Name and User Address
    } deriving (Generic, FromJSON, ToJSON, Show)

type NFTSchema = Endpoint "mint" NFTParams      -- mint Endpint with NFT parameters

mint :: NFTParams -> Contract w NFTSchema Text ()       -- Contract Monad
mint np = do
    utxos <- utxosAt $ npAddress np                     --UTXOAt take second field from param and returns all utxos
    case Map.keys utxos of                              -- maps the UTXO to proide Keys
        []       -> Contract.logError @String "no utxo found"   -- error out
        oref : _ -> do                                  -- if we find ANY then
            let tn      = npToken np                    -- Token Name
            let val     = Value.singleton (curSymbol oref tn) tn 1  -- Value, Curency Symbol, Token name, amount 1
                lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos    -- We don't need all UTXos, it will just look at all to find the one needed
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref           -- Tx must mint, must spend the tx output oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx        -- submit,
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx           -- wait for confirmation
            Contract.logInfo @String $ printf "forged %s" (show val)    -- log message

endpoints :: Contract () NFTSchema Text ()          -- endpoints are the same
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

test :: IO ()                                       -- This is basically the same emulator as before
test = runEmulatorTraceIO $ do
    let tn = "ABC"                                  -- pick Token name
        w1 = knownWallet 1                          -- specify 2 wallets
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints       
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"mint" h1 $ NFTParams             -- call endpoint for first wallet
        { npToken   = tn
        , npAddress = mockWalletAddress w1
        }
    callEndpoint @"mint" h2 $ NFTParams             -- repeat for second wallet
        { npToken   = tn
        , npAddress = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1

    -- Running this will create 2 NFTs with the same name,
    -- each wallet has it's own NFT

    -- Here are the results of the Emulator run in the Repl

{-
    
Wallet 7ce812d7a4770bbf58004067665c3a48f28ddd58: 
{, ""}: 99996890
{2d8911eaeda275b8e0c4ba484f1857435a3e5720fa9ac648fc343b57, "ABC"}: 1
Wallet 872cb83b5ee40eb23bfdab1772660c822a48d491: 
{, ""}: 99996890
{dd34121ddddfd43091b6f4368b4ea1715228bfb2e65558942bf052cc, "ABC"}: 1}

-}
