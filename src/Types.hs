module Types where

data VarData = Simple Char | Macro String
data Term = Variable VarData | Abstraction (Char, Term) | Application (Term, Term)

data Block = BlockText String | SubBlocks [Block]
instance Show Block where
    show (BlockText a) = a
    show (SubBlocks a) = show a

type IndexRange = (Int, Int)
