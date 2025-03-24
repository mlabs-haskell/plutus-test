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

import Data.Maybe (fromMaybe)
import Data.Monoid (Last (getLast))
import Optics (A_Lens, LabelOptic (labelOptic), lens, set, view)
import Plutus.ContextBuilder.Base (BaseBuilder, Builder (pack, _bb), unpack, yieldBaseTxInfo)
import Plutus.ContextBuilder.Internal (Normalizer (mkNormalized'), mkNormalized)
import PlutusLedgerApi.V3 (
  Credential (PubKeyCredential),
  Redeemer (Redeemer),
  ScriptContext (ScriptContext),
  ScriptInfo (RewardingScript),
  ToData (toBuiltinData),
 )

{- | A context builder for Rewarding. Corresponds to
 'Plutus.V1.Ledger.Contexts.Rewarding' specifically.

 @since WIP
-}
data RewardingBuilder = RB BaseBuilder (Maybe Credential)
  deriving stock
    ( -- | @since WIP
      Show
    )

-- | @since WIP
instance
  (k ~ A_Lens, a ~ BaseBuilder, b ~ BaseBuilder) =>
  LabelOptic "inner" k RewardingBuilder RewardingBuilder a b
  where
  labelOptic = lens (\(RB x _) -> x) $ \(RB _ cs) inner' -> RB inner' cs

-- | @since WIP
instance
  (k ~ A_Lens, a ~ Maybe Credential, b ~ Maybe Credential) =>
  LabelOptic "rewardingCred" k RewardingBuilder RewardingBuilder a b
  where
  labelOptic = lens (\(RB _ x) -> x) $ \(RB inner _) cs' -> RB inner cs'

-- | @since WIP
instance Semigroup RewardingBuilder where
  RB inner _ <> RB inner' cs@(Just _) =
    RB (inner <> inner') cs
  RB inner cs <> RB inner' Nothing =
    RB (inner <> inner') cs

-- | @since WIP
instance Monoid RewardingBuilder where
  mempty = RB mempty Nothing

-- | @since WIP
instance Builder RewardingBuilder where
  _bb = #inner
  pack x = set #inner x (mempty :: RewardingBuilder)

-- | @since WIP
instance Normalizer RewardingBuilder where
  mkNormalized' (RB bb cs) =
    RB (mkNormalized bb) cs

{- | Set CurrencySymbol for building Rewarding ScriptContext.

 @since WIP
-}
withRewarding :: Credential -> RewardingBuilder
withRewarding sc = RB mempty $ Just sc

{- | Builds @ScriptContext@ according to given configuration and
 @RewardingBuilder@.

 @since WIP
-}
buildRewarding' :: RewardingBuilder -> ScriptContext
buildRewarding' builder@(unpack -> bb) =
  let txinfo = yieldBaseTxInfo builder
      redeemer = fromMaybe (Redeemer $ toBuiltinData ()) $ getLast $ view #redeemer bb
      rewardCred = case view #rewardingCred builder of
        Just cred -> RewardingScript cred
        Nothing -> RewardingScript . PubKeyCredential $ ""
   in ScriptContext txinfo redeemer rewardCred
