module Types where

data VarData  = Simple Char | Macro String
data Term  = Variable VarData | Abstraction (Char, Term) | Application (Term, Term) | Normal Term | Empty
instance Show Term where
    show (Variable    (Simple v   )) = show v
    show (Variable    (Macro  text)) = text
    show (Abstraction (v, t)       ) = "\\" ++ show v ++ "." ++ show t
    show (Application (a, b)       ) = "(" ++ show a ++ ")(" ++ show b ++ ")"
    show (Normal      t            ) = "(" ++ show t ++ ")"
    show Empty                       = ""

instance Semigroup Term where
    (<>) a     Empty = a
    (<>) Empty b     = b
    (<>) a     b     = Application (a, b)

instance Monoid Term where
    mempty  = Empty
    mappend = (<>)

instance Foldable Term where
    foldMap f x = case x of
        Empty                    -> Empty
        (Normal      t         ) -> fmap f t
        (Application (a, b)    ) -> Application (fmap f a, fmap f b)
        (Abstraction (v, t)    ) -> Abstraction (f v, fmap f t)
        (Variable    (Simple v)) -> Variable (Simple (f v))
        (Variable    (Macro  m)) -> Variable (Macro m)


data Block = BlockText String | SubBlocks [Block]
instance Show Block where
    show (BlockText text) = show text
    show (SubBlocks bs  ) = show bs

type IndexRange = (Int, Int)
