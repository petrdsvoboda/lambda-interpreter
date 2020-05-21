module Types where

data VarData = Simple Char | Macro String
data Term = Variable VarData | Abstraction (Char, Term) | Application (Term, Term)

data Block = BlockText String | SubBlocks [Block]
instance Show Block where
    show (BlockText text) = show text
    show (SubBlocks bs  ) = show bs

type IndexRange = (Int, Int)
