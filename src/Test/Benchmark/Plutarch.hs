module Test.Benchmark.Plutarch (
  mkTermImplMetaData,
  sampleTerm,
  sampleTerm',
  pbenchAllSizesUniform,
  pbenchNonTinySizesRandomUniform,
  pbenchSizesRandomCached,
  pbenchSizesRandom,
) where

import Control.Monad.Primitive (MonadPrim)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Plutarch.Extra.Compile (mustCompile)
import System.Random (RandomGen)
import Test.Benchmark.Common (ImplData (..))
import Test.Benchmark.Plutus (
  BudgetExceeded,
  Costs,
  ImplMetaData,
  PlutusCostAxis,
  mkScriptImplMetaData,
  sampleScript,
 )
import Test.Benchmark.Precompile (CompiledTerm, compile', toScript)
import Test.Benchmark.Sized (
  SSample,
  SUniversalGen,
  benchAllSizesUniform,
  benchNonTinySizesRandomUniform,
  benchSizesRandom,
  benchSizesRandomCached,
 )

mkTermImplMetaData ::
  -- | Name of the implementation. Make sure it's unique.
  Text ->
  -- | The implementation without any inputs
  ClosedTerm a ->
  ImplMetaData
mkTermImplMetaData name term = mkScriptImplMetaData name $ mustCompile term

sampleTerm :: ClosedTerm a -> Either (BudgetExceeded PlutusCostAxis) Costs
sampleTerm term = sampleScript $ mustCompile term

sampleTerm' :: CompiledTerm a -> Either (BudgetExceeded PlutusCostAxis) Costs
sampleTerm' = sampleScript . toScript

-- | See 'benchAllSizesUniform'.
pbenchAllSizesUniform ::
  forall (a :: Type) (f :: S -> Type) (b :: S -> Type) (m :: Type -> Type) (s :: Type).
  ( Hashable a
  , MonadPrim s m
  ) =>
  -- | Size-dependent input domain generator.
  SUniversalGen a ->
  -- | Name of the function being benchmarked.
  Text ->
  -- | The function being benchmarked.
  ClosedTerm f ->
  -- | Applying the compiled function to the inputs.
  (CompiledTerm f -> a -> CompiledTerm b) ->
  -- | Desired sample size per input size.
  --
  -- The actual sample size will be exactly
  -- @min (cardinalityOfSize inputSize) desiredSampleSizePerInputSize@
  Int ->
  -- | The input sizes to benchmark with. Usually something like @[0..n]@.
  [Int] ->
  m (ImplData [SSample [Either (BudgetExceeded PlutusCostAxis) Costs]])
pbenchAllSizesUniform
  domainGen
  funName
  pfun
  applyPFun
  desiredSampleSizePerInputSize
  sizes =
    ImplData funName
      <$> benchAllSizesUniform
        domainGen
        (sampleTerm' . applyPFun pfun')
        desiredSampleSizePerInputSize
        sizes
    where
      pfun' = compile' pfun

-- | See 'benchNonTinySizesRandomUniform'.
pbenchNonTinySizesRandomUniform ::
  forall (a :: Type) (f :: S -> Type) (b :: S -> Type) (m :: Type -> Type) (s :: Type).
  ( Hashable a
  , MonadPrim s m
  ) =>
  -- | Size-dependent random input generator
  (forall (g :: Type). RandomGen g => Int -> g -> (a, g)) ->
  -- | Name of the function being benchmarked.
  Text ->
  -- | The function being benchmarked.
  ClosedTerm f ->
  -- | Applying the compiled function to the inputs.
  (CompiledTerm f -> a -> CompiledTerm b) ->
  -- | The sample size per input size.
  Int ->
  -- | The input sizes to benchmark with. Usually something like @[0..n]@.
  [Int] ->
  m (ImplData [SSample [Either (BudgetExceeded PlutusCostAxis) Costs]])
pbenchNonTinySizesRandomUniform
  randomGen
  funName
  pfun
  applyPFun
  sampleSizePerInputSize
  sizes =
    ImplData funName
      <$> benchNonTinySizesRandomUniform
        randomGen
        (sampleTerm' . applyPFun pfun')
        sampleSizePerInputSize
        sizes
    where
      pfun' = compile' pfun

-- | See 'benchSizesRandomCached'.
pbenchSizesRandomCached ::
  forall (a :: Type) (f :: S -> Type) (b :: S -> Type) (m :: Type -> Type) (s :: Type).
  ( Hashable a
  , MonadPrim s m
  ) =>
  -- | Size-dependent random input generator
  (forall (g :: Type). RandomGen g => Int -> g -> (a, g)) ->
  -- | Name of the function being benchmarked.
  Text ->
  -- | The function being benchmarked.
  ClosedTerm f ->
  -- | Applying the compiled function to the inputs.
  (CompiledTerm f -> a -> CompiledTerm b) ->
  -- | The sample size per input size.
  Int ->
  -- | The input sizes to benchmark with. Usually something like @[0..n]@.
  [Int] ->
  m (ImplData [SSample [Either (BudgetExceeded PlutusCostAxis) Costs]])
pbenchSizesRandomCached
  randomGen
  funName
  pfun
  applyPFun
  sampleSizePerInputSize
  sizes =
    ImplData funName
      <$> benchSizesRandomCached
        randomGen
        (sampleTerm' . applyPFun pfun')
        sampleSizePerInputSize
        sizes
    where
      pfun' = compile' pfun

-- | See 'benchSizesRandomCached'.
pbenchSizesRandom ::
  forall (a :: Type) (f :: S -> Type) (b :: S -> Type) (m :: Type -> Type) (s :: Type).
  ( Hashable a
  , MonadPrim s m
  ) =>
  -- | Size-dependent random input generator
  (forall (g :: Type). RandomGen g => Int -> g -> (a, g)) ->
  -- | Name of the function being benchmarked.
  Text ->
  -- | The function being benchmarked.
  ClosedTerm f ->
  -- | Applying the compiled function to the inputs.
  (CompiledTerm f -> a -> CompiledTerm b) ->
  -- | The sample size per input size.
  Int ->
  -- | The input sizes to benchmark with. Usually something like @[0..n]@.
  [Int] ->
  m (ImplData [SSample [Either (BudgetExceeded PlutusCostAxis) Costs]])
pbenchSizesRandom
  randomGen
  funName
  pfun
  applyPFun
  sampleSizePerInputSize
  sizes =
    ImplData funName
      <$> benchSizesRandom
        randomGen
        (sampleTerm' . applyPFun pfun')
        sampleSizePerInputSize
        sizes
    where
      pfun' = compile' pfun
