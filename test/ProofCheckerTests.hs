module ProofCheckerTests(
  allProofCheckerTests) where

import EFS
import ProofChecker
import TestUtils

allProofCheckerTests = do
  testFunction (checkProof simpleEFS) proofCases

simpleEFS =
  efs ["a", "b"] ["x", "y"] [("P", 1), ("K", 1)] [atomicFormula "P" 1 [str [letter "a"]]]

proofCases =
  [(sProof $ axiomStep $ atomicFormula "P" 1 [str [letter "a"]], correct),
   (sProof $ axiomStep $ atomicFormula "K" 1 [str [letter "b"]],
    notAxiom 1 $ atomicFormula "K" 1 [str [letter "b"]])]


