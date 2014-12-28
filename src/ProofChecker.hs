module ProofChecker(
  checkProof,
  correct,
  notAxiom) where

import Data.Map as M

import EFS

checkProof :: EFS -> Proof -> CheckResult
checkProof efs p = recCheckProof 0 M.empty efs p

recCheckProof :: Int -> Map Int Formula -> EFS -> Proof -> CheckResult
recCheckProof n fMap efs p = case isEmpty p of
  True -> Correct
  False -> case checkProofLine n fMap efs (firstLine p) of
    Left (newN, formulaMap) -> recCheckProof newN formulaMap efs (rest p)
    Right err -> err

checkProofLine :: Int ->
                  Map Int Formula ->
                  EFS ->
                  ProofLine ->
                  Either (Int, Map Int Formula) CheckResult
checkProofLine n fMap efs line = case justification line of
  AXIOM -> checkAxiom n fMap efs line

checkAxiom :: Int ->
              Map Int Formula ->
              EFS ->
              ProofLine ->
              Either (Int, Map Int Formula) CheckResult
checkAxiom n fMap efs line = Right Correct

type LineNo = Int

data CheckResult
  = Correct
  | NotAxiom LineNo Formula
    deriving (Eq, Show)

correct = Correct

notAxiom = NotAxiom
