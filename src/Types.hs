module Types where

data VarData = Simple Char | Macro String
data Term = Variable VarData | Abstraction (Char, Term) | Application (Term, Term)
instance Show Term where
    show (Variable    (Simple c   )) = show c
    show (Variable    (Macro  text)) = show text
    show (Abstraction (c, t)       ) = show c ++ "\n\t" ++ show t
    show (Application (a, b)       ) = show a ++ "\n" ++ show b

data Block = BlockText String | SubBlocks [Block]
instance Show Block where
    show (BlockText text) = show text
    show (SubBlocks bs  ) = show bs

type IndexRange = (Int, Int)
