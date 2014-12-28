module EFS(EFS,
           Proof,
           ProofLine,
           Formula,
           JustificationType(..),
           str,
           sProof,
           isEmpty,
           justification,
           rest,
           firstLine,
           letter,
           axiomStep,
           atomicFormula,
           emptyEFS) where

data EFS = EFS {}

data Formula
  = Atomic String Int [EFSString]
    deriving (Eq, Show)

atomicFormula predicateName degree arguments =
  Atomic predicateName degree arguments

data EFSString
  = EFSString [EFSSymbol]
    deriving (Eq, Show)

str = EFSString

data EFSSymbol
  = Letter String
    deriving (Eq, Show)

letter = Letter

emptyEFS = EFS

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
  = Axiom Formula
    deriving (Show)

axiomStep = Axiom

data JustificationType
  = AXIOM
    
justification :: ProofLine -> JustificationType
justification _ = AXIOM
