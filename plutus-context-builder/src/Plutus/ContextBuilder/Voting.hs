{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{- | Module: Plutus.ContextBuilder.Voting
 Copyright: (C) MLabs 2025
 Maintainer: Tomasz Maciosowski <tomasz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Builder for voting contexts. 'VotingBuilder' is an instance of 'Semigroup',
 which allows combining the results of this API's functions into a larger
 'VotingBuilder' using '<>'.

 @since 4.0.0
-}
module Plutus.ContextBuilder.Voting (
  -- * Types
  VotingBuilder,

  -- * Input
  withVoter,

  -- * builder
  buildVoting',
) where

import Data.Maybe (fromMaybe)
import Data.Monoid (Last (getLast))
import Optics (A_Lens, LabelOptic (labelOptic), lens, set, view)
import Plutus.ContextBuilder.Base (BaseBuilder, Builder (pack, _bb), unpack, yieldBaseTxInfo)
import Plutus.ContextBuilder.Internal (Normalizer (mkNormalized'), mkNormalized)
import PlutusLedgerApi.V3 (
  PubKeyHash (PubKeyHash),
  Redeemer (Redeemer),
  ScriptContext (ScriptContext),
  ScriptInfo (VotingScript),
  ToData (toBuiltinData),
  Voter (StakePoolVoter),
 )

{- | A context builder for Rewarding. Corresponds to
 'Plutus.V1.Ledger.Contexts.Rewarding' specifically.

 @since 4.0.0
-}
data VotingBuilder = VB BaseBuilder (Maybe Voter)
  deriving stock
    ( -- | @since 4.0.0
      Show
    )

-- | @since 4.0.0
instance
  (k ~ A_Lens, a ~ BaseBuilder, b ~ BaseBuilder) =>
  LabelOptic "inner" k VotingBuilder VotingBuilder a b
  where
  labelOptic = lens (\(VB x _) -> x) $ \(VB _ v) inner' -> VB inner' v

-- | @since 4.0.0
instance
  (k ~ A_Lens, a ~ Maybe Voter, b ~ Maybe Voter) =>
  LabelOptic "voter" k VotingBuilder VotingBuilder a b
  where
  labelOptic = lens (\(VB _ x) -> x) $ \(VB inner _) v' -> VB inner v'

-- | @since 4.0.0
instance Semigroup VotingBuilder where
  VB inner _ <> VB inner' cs@(Just _) =
    VB (inner <> inner') cs
  VB inner cs <> VB inner' Nothing =
    VB (inner <> inner') cs

-- | @since 4.0.0
instance Monoid VotingBuilder where
  mempty = VB mempty Nothing

-- | @since 4.0.0
instance Builder VotingBuilder where
  _bb = #inner
  pack x = set #inner x (mempty :: VotingBuilder)

-- | @since 4.0.0
instance Normalizer VotingBuilder where
  mkNormalized' (VB bb cs) =
    VB (mkNormalized bb) cs

{- | Set Voter for building Voting ScriptContext.

 @since 4.0.0
-}
withVoter :: Voter -> VotingBuilder
withVoter v = VB mempty $ Just v

{- | Builds @ScriptContext@ according to given configuration and
 @VotingBuilder@.

 @since 4.0.0
-}
buildVoting' :: VotingBuilder -> ScriptContext
buildVoting' builder@(unpack -> bb) =
  let txinfo = yieldBaseTxInfo builder
      redeemer = fromMaybe (Redeemer $ toBuiltinData ()) $ getLast $ view #redeemer bb
      rewardCred = case view #voter builder of
        Just v -> VotingScript v
        Nothing -> VotingScript $ StakePoolVoter $ PubKeyHash ""
   in ScriptContext txinfo redeemer rewardCred
