{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{- | Module: Plutus.ContextBuilder.Rewarding
 Copyright: (C) Liqwid Labs 2022
 Maintainer: Seungheon Oh <seungheon@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Builder for rewarding contexts. 'RewardingBuilder' is an instance of 'Semigroup',
 which allows combining the results of this API's functions into a larger
 'RewardingBuilder' using '<>'.
-}
module Plutus.ContextBuilder.Rewarding (
  -- * Types
  RewardingBuilder,

  -- * Input
  withRewarding,

  -- * builder
  buildRewarding',
) where

import Data.Foldable (Foldable (toList))
import Optics (A_Lens, LabelOptic (labelOptic), lens, set, view)
import Plutus.ContextBuilder.Base (
  BaseBuilder,
  Builder (pack, _bb),
  unpack,
  yieldBaseTxInfo,
  yieldExtraDatums,
  yieldInInfoDatums,
  yieldMint,
  yieldOutDatums,
  yieldRedeemerMap,
 )
import Plutus.ContextBuilder.Internal (Normalizer (mkNormalized'), mkNormalized)
import PlutusLedgerApi.V3 (
  Credential (PubKeyCredential),
  Redeemer,
  ScriptContext (ScriptContext),
  ScriptInfo (RewardingScript),
  TxInfo (
    txInfoData,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoRedeemers,
    txInfoReferenceInputs,
    txInfoSignatories,
    txInfoTxCerts,
    txInfoWdrl
  ),
  Value (getValue),
 )
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx.AssocMap qualified as AssocMap

{- | A context builder for Rewarding. Corresponds to
 'Plutus.V1.Ledger.Contexts.Rewarding' specifically.

 @since 2.8.0
-}
data RewardingBuilder = RB BaseBuilder (Maybe Credential)
  deriving stock
    ( -- | @since 2.8.0
      Show
    )

-- | @since 2.8.0
instance
  (k ~ A_Lens, a ~ BaseBuilder, b ~ BaseBuilder) =>
  LabelOptic "inner" k RewardingBuilder RewardingBuilder a b
  where
  labelOptic = lens (\(RB x _) -> x) $ \(RB _ cs) inner' -> RB inner' cs

-- | @since 2.8.0
instance
  (k ~ A_Lens, a ~ Maybe Credential, b ~ Maybe Credential) =>
  LabelOptic "rewardingCred" k RewardingBuilder RewardingBuilder a b
  where
  labelOptic = lens (\(RB _ x) -> x) $ \(RB inner _) cs' -> RB inner cs'

-- | @since 2.8.0
instance Semigroup RewardingBuilder where
  RB inner _ <> RB inner' cs@(Just _) =
    RB (inner <> inner') cs
  RB inner cs <> RB inner' Nothing =
    RB (inner <> inner') cs

-- | @since 2.8.0
instance Monoid RewardingBuilder where
  mempty = RB mempty Nothing

-- | @since 2.8.0
instance Builder RewardingBuilder where
  _bb = #inner
  pack x = set #inner x (mempty :: RewardingBuilder)

-- | @since 2.8.0
instance Normalizer RewardingBuilder where
  mkNormalized' (RB bb cs) =
    RB (mkNormalized bb) cs

{- | Set CurrencySymbol for building Rewarding ScriptContext.

 @since 2.8.0
-}
withRewarding :: Credential -> RewardingBuilder
withRewarding sc = RB mempty $ Just sc

{- | Builds @ScriptContext@ according to given configuration and
 @RewardingBuilder@.

 @since 2.8.0
-}
buildRewarding' ::
  Redeemer ->
  RewardingBuilder ->
  ScriptContext
buildRewarding' redeemer builder@(unpack -> bb) =
  let (ins, inDat) = yieldInInfoDatums . view #inputs $ bb
      (refin, _) = yieldInInfoDatums . view #referenceInputs $ bb
      (outs, outDat) = yieldOutDatums . view #outputs $ bb
      mintedValue = yieldMint . view #mints $ bb
      extraDat = yieldExtraDatums . view #datums $ bb
      base = yieldBaseTxInfo builder
      redeemerMap = yieldRedeemerMap (view #inputs bb) (view #mints bb)
      txinfo =
        base
          { txInfoInputs = ins
          , txInfoReferenceInputs = refin
          , txInfoOutputs = outs
          , txInfoData = AssocMap.unsafeFromList $ inDat <> outDat <> extraDat
          , txInfoMint = UnsafeMintValue $ getValue mintedValue
          , txInfoRedeemers = AssocMap.unsafeFromList $ toList (view #redeemers bb) <> redeemerMap
          , txInfoSignatories = toList . view #signatures $ bb
          , txInfoWdrl = AssocMap.unsafeFromList $ toList (view #withdrawals bb)
          , txInfoTxCerts = toList (view #txCerts bb)
          }
      rewardCred = case view #rewardingCred builder of
        Just cred -> RewardingScript cred
        Nothing -> RewardingScript . PubKeyCredential $ ""
   in ScriptContext txinfo redeemer rewardCred
