module Plutarch.Test.Eval (evalScript) where

-- Adapted from Plutarch.Internal.Evaluate

import Data.Text (Text)
import PlutusCore qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudget (
  ExBudget (ExBudget),
  ExRestrictingBudget (ExRestrictingBudget),
  minusExBudget,
 )
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCekParameters)
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (ExCPU), ExMemory (ExMemory))
import PlutusCore.Version (plcVersion100)
import UntypedPlutusCore (
  Program (Program),
  Term,
 )
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as Cek

type EvalError = (Cek.CekEvaluationException PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun)

-- | Evaluate a script with a big budget, returning the trace log and term result.
evalScript ::
  UPLC.Program UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun () ->
  (Either EvalError (UPLC.Program UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()), ExBudget, [Text])
evalScript = evalScript' budget
  where
    -- from https://github.com/input-output-hk/cardano-node/blob/master/configuration/cardano/mainnet-alonzo-genesis.json#L17
    budget = ExBudget (ExCPU 10000000000) (ExMemory 10000000)

-- | Evaluate a script with a specific budget, returning the trace log and term result.
evalScript' ::
  ExBudget ->
  UPLC.Program UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun () ->
  ( Either
      (Cek.CekEvaluationException PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun)
      (UPLC.Program UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ())
  , ExBudget
  , [Text]
  )
evalScript' budget (Program _ _ t) = case evalTerm budget (UPLC.termMapNames UPLC.fakeNameDeBruijn t) of
  (res, remaining, logs) -> (Program () plcVersion100 . UPLC.termMapNames UPLC.unNameDeBruijn <$> res, remaining, logs)

evalTerm ::
  ExBudget ->
  Term PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun () ->
  ( Either
      EvalError
      (Term PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun ())
  , ExBudget
  , [Text]
  )
evalTerm budget t =
  case Cek.runCekDeBruijn defaultCekParameters (Cek.restricting (ExRestrictingBudget budget)) Cek.logEmitter t of
    (errOrRes, Cek.RestrictingSt (ExRestrictingBudget final), logs) -> (errOrRes, budget `minusExBudget` final, logs)
