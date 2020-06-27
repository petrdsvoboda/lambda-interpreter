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

data Term  = Empty | Variable String | Macro String | Abstraction (String, Term ) | Application (Term, Term) deriving (Eq)
instance Show Term where
    show (Variable    v     ) = v
    show (Macro       text  ) = text
    show (Abstraction (v, t)) = "(\\" ++ v ++ "." ++ show t ++ ")"
    show (Application (a, b)) = show a ++ " " ++ show b
    show Empty                = ""

instance Semigroup Term where
    (<>) a     Empty = a
    (<>) Empty b     = b
    (<>) a     b     = Application (a, b)

instance Monoid Term where
    mempty  = Empty
    mappend = (<>)

-- instance Foldable Term where
--     foldMap f Empty                   = mempty
--     foldMap f (  Application (a, b )) = foldMap f a <> foldMap f b
--     foldMap f t@(Abstraction (v, t2)) = foldMap f a <> foldMap f b
--     foldMap f t                       = f t

-- instance Functor Term where
--     fmap f x = case x of
--         Empty                -> Empty
--         (Variable    v     ) -> Variable (f v)
--         (Application (a, b)) -> fmap f a <> fmap f b
--         t                    -> f t

-- instance Applicative Term where
--     pure = Variable
--     (<*>) _  Empty = Empty
--     (<*>) ft xs    = case ft of
--         Empty              -> Empty
--         Macro       m      -> Macro m
--         Variable    f      -> fmap f xs
--         Application (a, b) -> Application (a <*> xs, b <*> xs)
--         Abstraction (v, t) -> t <*> xs


-- instance Monad Term where
--     (>>=) Empty f = Empty
--     (>>=) x     f = case x of
--         Empty                -> Empty
--         (Macro       m     ) -> Macro m
--         (Variable    v     ) -> f v
--         (Application (a, b)) -> Application (a >>= f, b >>= f)
--         (Abstraction (v, t)) -> t >>= f


data Block = BlockText String | SubBlocks [Block]
instance Show Block where
    show (BlockText text) = show text
    show (SubBlocks bs  ) = show bs

type IndexRange = (Int, Int)
