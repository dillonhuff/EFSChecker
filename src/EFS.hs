module EFS(EFS,
           Proof,
           ProofLine,
           Formula,
           JustificationType(..),
           LineNo,
           sProof,
           proof,
           isEmpty,
           justification,
           sentence,
           rest,
           firstLine,
           letter,
           var,
           axiomStep,
           substitutionStep,
           sFormula,
           formula,
           atom,
           term,
           efs,
           isAxiom) where

import Data.List as L
import Data.Set as S

type LineNo = Int

data EFS = EFS {
  letters :: [Symbol],
  variables :: [Symbol],
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
  = Formula [Atom]
    deriving (Eq, Ord, Show)

formula = Formula
sFormula predName degree terms = Formula [atom predName degree terms]

data Atom
  = Atom String Int [Term]
    deriving (Eq, Ord, Show)

atom predicateName degree arguments =
  Atom predicateName degree arguments

data Term
  = Term [Symbol]
    deriving (Eq, Ord, Show)

term = Term

data Symbol
  = Letter String
  | Variable String
    deriving (Eq, Ord, Show)

letter = Letter
var = Variable

data Proof = Proof [ProofLine]
             deriving (Show)

proof = Proof
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

substitutionStep f lineNo var@(Variable x) term =
  ProofLine f (Substitution lineNo var term)
substitutionStep _ _ s _ = error $ show s ++ " is not a variable"

sentence (ProofLine f _) = f

data Justification
  = Axiom
  | Substitution LineNo Symbol Term
    deriving (Show)

data JustificationType
  = AXIOM
  | SUBSTITUTION
    
justification :: ProofLine -> JustificationType
justification (ProofLine _ Axiom) = AXIOM
justification (ProofLine _ (Substitution _ _ _)) = SUBSTITUTION
