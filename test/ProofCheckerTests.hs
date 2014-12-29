module ProofCheckerTests(
  allProofCheckerTests) where

import EFS
import ProofChecker
import TestUtils

allProofCheckerTests = do
  testFunction (checkProof simpleEFS) proofCases

simpleEFS =
  efs ["a", "b"]
      ["x", "y"]
      [("P", 1), ("K", 1)]
      [sFormula "P" 1 [term [letter "a"]],
       formula [atom "P" 1 [term [var "x"]],
                        atom "P" 1 [term [var "y"]],
                        atom "P" 1 [term [var "x", var "y"]]]]

proofCases =
  [(sProof $ axiomStep $ sFormula "P" 1 [term [letter "a"]], correct),
   (sProof $ axiomStep $ sFormula "K" 1 [term [letter "b"]],
    notAxiom 1 $ sFormula "K" 1 [term [letter "b"]])]


