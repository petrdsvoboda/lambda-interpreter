 {-# LANGUAGE FlexibleInstances #-}
module Types where

data SeparatorToken = Begin | End deriving (Eq)
data KeywordToken = Assign | Fn | EndFn deriving (Eq)
data Token = Identifier String | Separator SeparatorToken | Keyword KeywordToken deriving (Eq)
type SavedMacro = (String, String)

instance Show Token where
    show (Identifier x  ) = "ID~" ++ x
    show (Separator  sep) = case sep of
        Begin -> "S("
        End   -> "S)"
    show (Keyword key) = case key of
        Assign -> "K="
        Fn     -> "K\\"
        EndFn  -> "K."
instance {-# OVERLAPPING #-} Show [Token] where
    show = foldl (\acc curr -> acc ++ " " ++ show curr) ""

data Term  = Empty | Variable String | Macro String | Abstraction ([String], Term ) | Application [Term]
type Expr = (Term, Maybe String)
type EvalRes = Either String Term

instance Show Term where
    show (Variable    v      ) = v
    show (Macro       text   ) = text
    show (Abstraction (vs, t)) = "(\\" ++ unwords vs ++ "." ++ inner ++ ")"
      where
        inner = case t of
            Application ts -> unwords $ map show ts
            _              -> show t
    show (Application ts) = "(" ++ (unwords $ map show ts) ++ ")"
    show Empty            = ""

instance Eq Term where
    (==) Empty         Empty         = True
    (==) (Variable v1) (Variable v2) = v1 == v2
    (==) (Macro    m1) (Macro    m2) = m1 == m2
    (==) (Abstraction (vs1, t1)) (Abstraction (vs2, t2)) =
        vs1 == vs2 && t1 == t2
    (==) (Application (t1 : ts1)) (Application (t2 : ts2)) =
        t1 == t2 && Application ts1 == Application ts2
    (==) (Application [t1]) t2                 = t1 == t2
    (==) t1                 (Application [t2]) = t1 == t2
    (==) (Application [])   (Application []  ) = True
    (==) _                  _                  = False

instance Semigroup Term where
    (<>) a                     Empty                 = a
    (<>) Empty                 b                     = b
    (<>) (Application as     ) (Application bs)      = Application (as ++ bs)
    (<>) (Application as     ) b                     = Application (as ++ [b])
    (<>) (Abstraction ([], t)) b                     = t <> b
    (<>) a                     (Application bs     ) = Application (a : bs)
    (<>) a                     (Abstraction ([], t)) = a <> t
    (<>) a                     b                     = Application [a, b]

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
