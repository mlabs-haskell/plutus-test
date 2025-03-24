{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{- | Module: Plutus.ContextBuilder.Rewarding
 Copyright: (C) Liqwid Labs 2022
 Copyright: (C) MLabs 2025
 Maintainer: Tomasz Maciosowski <tomasz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Builder for rewarding contexts. 'CertifyingBuilder' is an instance of 'Semigroup',
 which allows combining the results of this API's functions into a larger
 'CertifyingBuilder' using '<>'.

 @since 4.0.0
-}
module Plutus.ContextBuilder.Certifying (
  -- * Types
  CertifyingBuilder,

  -- * Input
  withCertifying,

  -- * builder
  buildCertifying',
) where

import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (getLast))
import Optics (A_Lens, LabelOptic (labelOptic), lens, set, view)
import Plutus.ContextBuilder.Base (BaseBuilder, Builder (pack, _bb), unpack, yieldBaseTxInfo)
import Plutus.ContextBuilder.Internal (Normalizer (mkNormalized'), mkNormalized)
import PlutusLedgerApi.V3 (
  Credential (PubKeyCredential),
  Redeemer (Redeemer),
  ScriptContext (ScriptContext),
  ScriptInfo (CertifyingScript),
  ToData (toBuiltinData),
  TxCert (TxCertRegStaking),
  TxInfo (
    txInfoTxCerts
  ),
 )

{- | A context builder for Certifying. Corresponds to
 'Plutus.V1.Ledger.Contexts.Certifying' specifically.

 @since 4.0.0
-}
data CertifyingBuilder = CB BaseBuilder (Maybe TxCert)
  deriving stock
    ( -- | @since 4.0.0
      Show
    )

-- | @since 4.0.0
instance
  (k ~ A_Lens, a ~ BaseBuilder, b ~ BaseBuilder) =>
  LabelOptic "inner" k CertifyingBuilder CertifyingBuilder a b
  where
  labelOptic = lens (\(CB x _) -> x) $ \(CB _ cs) inner' -> CB inner' cs

-- | @since 4.0.0
instance
  (k ~ A_Lens, a ~ Maybe TxCert, b ~ Maybe TxCert) =>
  LabelOptic "certifyingTxCert" k CertifyingBuilder CertifyingBuilder a b
  where
  labelOptic = lens (\(CB _ x) -> x) $ \(CB inner _) cs' -> CB inner cs'

-- | @since 4.0.0
instance Semigroup CertifyingBuilder where
  CB inner _ <> CB inner' cs@(Just _) =
    CB (inner <> inner') cs
  CB inner cs <> CB inner' Nothing =
    CB (inner <> inner') cs

-- | @since 4.0.0
instance Monoid CertifyingBuilder where
  mempty = CB mempty Nothing

-- | @since 4.0.0
instance Builder CertifyingBuilder where
  _bb = #inner
  pack x = set #inner x (mempty :: CertifyingBuilder)

-- | @since 4.0.0
instance Normalizer CertifyingBuilder where
  mkNormalized' (CB bb cs) =
    CB (mkNormalized bb) cs

{- | Set DCert for building Certifying ScriptContext.

 @since 4.0.0
-}
withCertifying :: TxCert -> CertifyingBuilder
withCertifying sc = CB mempty $ Just sc

{- | Builds @ScriptContext@ according to given configuration and
 @CertifyingBuilder@.

 @since 4.0.0
-}
buildCertifying' ::
  CertifyingBuilder ->
  ScriptContext
buildCertifying' builder@(unpack -> bb) =
  let txinfo = yieldBaseTxInfo builder
      redeemer = fromMaybe (Redeemer $ toBuiltinData ()) $ getLast $ view #redeemer bb
      scriptInfo = case view #certifyingTxCert builder of
        Just txCert -> CertifyingScript (maybe 0 fst $ find ((== txCert) . snd) . zip [0 ..] $ txInfoTxCerts txinfo) txCert
        Nothing -> CertifyingScript 0 (TxCertRegStaking (PubKeyCredential "") Nothing)
   in ScriptContext txinfo redeemer scriptInfo
