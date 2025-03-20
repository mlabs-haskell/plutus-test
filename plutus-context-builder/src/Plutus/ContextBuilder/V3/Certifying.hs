{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{- | Module: Plutus.ContextBuilder.V3.Certifying
Copyright: (C) Liqwid Labs 2022
Maintainer: Seungheon Oh <seungheon@mlabs.city>
Portability: GHC only
Stability: Experimental

Builder for rewarding contexts. 'CertifyingBuilder' is an instance of 'Semigroup',
which allows combining the results of this API's functions into a larger
'CertifyingBuilder' using '<>'.
-}
module Plutus.ContextBuilder.V3.Certifying (
  -- * Types
  CertifyingBuilder,

  -- * Input
  withCertifying,

  -- * builder
  buildCertifying',
)
where

import Data.Foldable (Foldable (toList))
import Optics (A_Lens, LabelOptic (labelOptic), lens, set, view)
import Plutus.ContextBuilder.V3.Base (
  BaseBuilder,
  Builder (pack, _bb),
  unpack,
  yieldBaseTxInfo,
  yieldExtraDatums,
  yieldInInfoDatums,
  yieldMint,
  yieldOutDatums,
  yieldRedeemer,
  yieldRedeemerMap,
 )
import Plutus.ContextBuilder.V3.Internal (Normalizer (mkNormalized'), mkNormalized)
import PlutusLedgerApi.V3 (
  Credential (PubKeyCredential),
  ScriptContext (ScriptContext),
  ScriptInfo (CertifyingScript),
  TxCert (TxCertRegStaking),
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
 )
import PlutusTx.AssocMap qualified as AssocMap

{- | A context builder for Certifying. Corresponds to
'Plutus.V1.Ledger.Contexts.Certifying' specifically.

@since 3.0.0
-}
data CertifyingBuilder = CB BaseBuilder (Maybe (Integer, TxCert))
  deriving stock
    ( -- | @since 3.0.0
      Show
    )

-- | @since 3.0.0
instance
  (k ~ A_Lens, a ~ BaseBuilder, b ~ BaseBuilder) =>
  LabelOptic "inner" k CertifyingBuilder CertifyingBuilder a b
  where
  labelOptic = lens (\(CB x _) -> x) $ \(CB _ cs) inner' -> CB inner' cs

-- | @since 3.0.0
instance
  (k ~ A_Lens, a ~ Maybe (Integer, TxCert), b ~ Maybe (Integer, TxCert)) =>
  LabelOptic "certifyingTxCert" k CertifyingBuilder CertifyingBuilder a b
  where
  labelOptic = lens (\(CB _ x) -> x) $ \(CB inner _) cs' -> CB inner cs'

-- | @since 3.0.0
instance Semigroup CertifyingBuilder where
  CB inner _ <> CB inner' cs@(Just _) =
    CB (inner <> inner') cs
  CB inner cs <> CB inner' Nothing =
    CB (inner <> inner') cs

-- | @since 3.0.0
instance Monoid CertifyingBuilder where
  mempty = CB mempty Nothing

-- | @since 3.0.0
instance Builder CertifyingBuilder where
  _bb = #inner
  pack x = set #inner x (mempty :: CertifyingBuilder)

-- | @since 3.0.0
instance Normalizer CertifyingBuilder where
  mkNormalized' (CB bb cs) =
    CB (mkNormalized bb) cs

{- | Set TxCert for building Certifying ScriptContext.

@since 3.0.0
-}
withCertifying :: Integer -> TxCert -> CertifyingBuilder
withCertifying certIdx txCert = CB mempty $ Just (certIdx, txCert)

{- | Builds @ScriptContext@ according to given configuration and
@CertifyingBuilder@.

@since 3.0.0
-}
buildCertifying' ::
  CertifyingBuilder ->
  ScriptContext
buildCertifying' builder@(unpack -> bb) =
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
          , txInfoMint = mintedValue
          , txInfoRedeemers = AssocMap.unsafeFromList $ toList (view #redeemers bb) <> redeemerMap
          , txInfoSignatories = toList . view #signatures $ bb
          , txInfoWdrl = AssocMap.unsafeFromList $ toList (view #withdrawals bb)
          , txInfoTxCerts = toList (view #txcerts bb)
          }
      -- TODO: verify certifyingTxCert in txInfoTxCerts, and add the index automatically
      rewardCred = case view #certifyingTxCert builder of
        Just (certIdx, txCert) -> CertifyingScript certIdx txCert
        Nothing -> CertifyingScript 0 $ TxCertRegStaking (PubKeyCredential "") Nothing

      redeemer = yieldRedeemer . view #redeemer $ bb
   in ScriptContext txinfo redeemer rewardCred
