module EFS(EFS,
           Proof,
           ProofLine,
           Formula,
           JustificationType(..),
           str,
           sProof,
           isEmpty,
           justification,
           sentence,
           rest,
           firstLine,
           letter,
           axiomStep,
           atomicFormula,
           efs,
           isAxiom) where

import Data.List as L
import Data.Set as S

data EFS = EFS {
  letters :: [EFSSymbol],
  variables :: [EFSSymbol],
  predicates :: [(String, Int)],
  axioms :: Set Formula
  } deriving (Show)

efs :: [String] ->
       [String] ->
       [(String, Int)] ->
       [Formula] ->
       EFS
efs letterNames varNames predicates axioms =
  EFS (L.map Letter letterNames) (L.map Variable varNames) predicates (S.fromList axioms)


isAxiom :: EFS -> Formula -> Bool
isAxiom efs f = S.member f (axioms efs)

data Formula
  = Atomic String Int [EFSString]
    deriving (Eq, Ord, Show)

atomicFormula predicateName degree arguments =
  Atomic predicateName degree arguments

data EFSString
  = EFSString [EFSSymbol]
    deriving (Eq, Ord, Show)

str = EFSString

data EFSSymbol
  = Letter String
  | Variable String
    deriving (Eq, Ord, Show)

letter = Letter

data Proof = Proof [ProofLine]
             deriving (Show)

sProof x = Proof [x]

isEmpty :: Proof -> Bool
isEmpty (Proof []) = True
isEmpty _ = False

firstLine :: Proof -> ProofLine
firstLine (Proof (l:ls)) = l

rest :: Proof -> Proof
rest (Proof (l:ls)) = Proof ls

data ProofLine
  = ProofLine Formula Justification
    deriving (Show)

axiomStep f = ProofLine f Axiom

sentence (ProofLine f _) = f

data Justification
  = Axiom
    deriving (Show)

data JustificationType
  = AXIOM
    
justification :: ProofLine -> JustificationType
justification _ = AXIOM
