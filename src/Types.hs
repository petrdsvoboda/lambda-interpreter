 {-# LANGUAGE FlexibleInstances #-}
module Types where

import           Test.QuickCheck
import           Control.Monad

data SeparatorToken = Begin | End deriving (Eq)
data KeywordToken = Assign | Fn | EndFn deriving (Eq)
data Token = Identifier String | Separator SeparatorToken | Keyword KeywordToken deriving (Eq)
type MacroHeap = [(String, String, Term)]

data Term  = Empty | Variable String | Macro String | Abstraction ([String], Term ) | Application [Term]
type Expr = (Term, Maybe String)
type EvalRes = Either String Term
data StackItem = StackItem { isFn::Bool, inFn::Bool, fnVar::[String], term::Term}

type Error = String
type Answer = String
type ProgramStep = Either Error Answer
data ProgramFlags = ProgramFlags { quiet :: Bool }

instance Show Token where
    show (Identifier x  ) = "ID~" ++ x
    show (Separator  sep) = case sep of
        Begin -> "S("
        End   -> "S)"
    show (Keyword key) = case key of
        Assign   -> "K="
        Types.Fn -> "K\\"
        EndFn    -> "K."
instance {-# OVERLAPPING #-} Show [Token] where
    show = foldl (\acc curr -> acc ++ " " ++ show curr) ""

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

instance Arbitrary Term where
    arbitrary = do
        depth <- choose (0, 3)
        arbitrary' depth
      where
        arbitrary' :: Integer -> Gen Term
        arbitrary' depth = if depth > 0
            then oneof [empty, var, abs, ap]
            else return Empty
          where
            empty = return Empty
            var   = do
                len <- (choose (1, 3)) :: Gen Integer
                var <- sequence [ elements ['a' .. 'z'] | _ <- [0 .. len] ]
                return $ Variable var
            abs = do
                len <- (choose (1, 3)) :: Gen Integer
                let genVar =
                        sequence [ elements ['a' .. 'z'] | _ <- [0 .. len] ]
                varLen <- (choose (1, 3)) :: Gen Integer
                vars   <- sequence [ genVar | _ <- [0 .. varLen] ]
                t      <- arbitrary' (depth - 1)
                return $ Abstraction (vars, t)
            ap = do
                len <- (choose (1, 5)) :: Gen Integer
                ts  <- sequence [ arbitrary' (depth - 1) | _ <- [0 .. len] ]
                return $ Application ts


instance Monoid Term where
    mempty  = Empty
    mappend = (<>)


-- instance {-# OVERLAPPING #-} Semigroup (Maybe Term) where
--     Nothing <> _       = Nothing
--     _       <> Nothing = Nothing
--     Just a  <> Just b  = Just (a <> b)

instance {-# OVERLAPPING #-} Semigroup (Either String Term) where
    Left err1 <> Left err2 = Left (err1 ++ "\n" ++ err2)
    Left err  <> _         = Left err
    _         <> Left  err = Left err
    Right x   <> Right y   = Right (x <> y)

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


