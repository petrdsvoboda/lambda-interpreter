 {-# LANGUAGE FlexibleInstances #-}
module Types where

data SeparatorToken = Begin | End deriving (Eq)
data KeywordToken = Fn | EndFn deriving (Eq)
data Token = Identifier String | Separator SeparatorToken | Keyword KeywordToken deriving (Eq)

instance Show Token where
    show (Identifier x  ) = "ID~" ++ x
    show (Separator  sep) = case sep of
        Begin -> "S("
        End   -> "S)"
    show (Keyword key) = case key of
        Fn    -> "K\\"
        EndFn -> "K."
instance {-# OVERLAPPING #-} Show [Token] where
    show = foldl (\acc curr -> acc ++ " " ++ show curr) ""

data Term a  = Empty | Variable a | Macro String | Abstraction (a, Term a) | Application (Term a, Term a)
instance Show a => Show (Term a) where
    show (Variable    v     ) = "(" ++ show v ++ ")"
    show (Macro       text  ) = text
    show (Abstraction (v, t)) = "(\\" ++ show v ++ "." ++ show t ++ ")"
    show (Application (a, b)) = show a ++ show b
    show Empty                = ""

instance Semigroup (Term a) where
    (<>) a     Empty = a
    (<>) Empty b     = b
    (<>) a     b     = Application (a, b)

instance Monoid (Term a) where
    mempty  = Empty
    mappend = (<>)

instance Functor Term where
    fmap f x = case x of
        Empty                -> Empty
        (Macro       m     ) -> Macro m
        (Variable    v     ) -> Variable (f v)
        (Application (a, b)) -> Application (fmap f a, fmap f b)
        (Abstraction (v, t)) -> Abstraction (f v, fmap f t)

instance Applicative Term where
    pure = Variable
    (<*>) _  Empty = Empty
    (<*>) ft xs    = case ft of
        Empty              -> Empty
        Macro       m      -> Macro m
        Variable    f      -> fmap f xs
        Application (a, b) -> Application (a <*> xs, b <*> xs)
        Abstraction (v, t) -> t <*> xs


instance Monad Term where
    (>>=) Empty f = Empty
    (>>=) x     f = case x of
        Empty                -> Empty
        (Macro       m     ) -> Macro m
        (Variable    v     ) -> f v
        (Application (a, b)) -> Application (a >>= f, b >>= f)
        (Abstraction (v, t)) -> t >>= f


data Block = BlockText String | SubBlocks [Block]
instance Show Block where
    show (BlockText text) = show text
    show (SubBlocks bs  ) = show bs

type IndexRange = (Int, Int)
