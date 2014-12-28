module ProofCheckerTests(
  allProofCheckerTests) where

import EFS
import ProofChecker
import TestUtils

allProofCheckerTests = do
  testFunction (checkProof simpleEFS) proofCases

simpleEFS = emptyEFS

proofCases =
  [(sProof $ axiomStep $ atomicFormula "P" 1 [str [letter "a"]], correct),
   (sProof $ axiomStep $ atomicFormula "K" 1 [str [letter "b"]],
    notAxiom 1 $ atomicFormula "K" 1 [str [letter "b"]])]


