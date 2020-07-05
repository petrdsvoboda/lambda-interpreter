 {-# LANGUAGE FlexibleInstances #-}
module Types where

-- | Used to separate block in terms - (, )
data SeparatorToken = Begin | End deriving (Eq)
-- | Used to mark keywords in terms - \, ., =
data KeywordToken = Assign | Fn | EndFn deriving (Eq)
-- | All possible tokens that can be parsed
data Token = Identifier String | Separator SeparatorToken | Keyword KeywordToken deriving (Eq)
-- | Contains all macro mappings
-- (identifier, string version of term, term)
type MacroHeap = [(String, String, Term)]

-- | Term structure
-- Empty
-- Variable
-- Application
-- Abstraction
data Term  = Empty | Variable String | Macro String | Abstraction ([String], Term ) | Application [Term]
-- | Identifies assignment
-- assignment - (term, Just identifier)
-- no assignment - (term, Nothing)
type Expr = (Term, Maybe String)
type Error = String
-- | Result of one step of the evaluation
type EvalRes = Either Error Term
-- | Saves data of token context
data StackItem = StackItem {
    isFn::Bool, -- ^ term is going to be abstraction
    inFn::Bool, -- ^ pass identifiers as arguments (True) or as variables (False)
    fnVar::[String], -- ^ abstarction arguments
    term::Term -- ^ inner term
}

type Answer = String
-- | Result of one step in CLI
type ProgramStep = Either Error Answer
-- | Flags that can set to program
data ProgramFlags = ProgramFlags {
    quiet :: Bool -- ^ don't show intermediate steps
}

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

-- | Simple equality, ignore single applications
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

-- | Merge Applications
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

instance {-# OVERLAPPING #-} Semigroup (Maybe Term) where
    -- | Return term only if no Nothing
    Nothing <> _       = Nothing
    _       <> Nothing = Nothing
    Just a  <> Just b  = Just (a <> b)

instance {-# OVERLAPPING #-} Semigroup (Either String Term) where
    -- | Merge errors, don't return term if there is error
    Left err1 <> Left err2 = Left (err1 ++ "\n" ++ err2)
    Left err  <> _         = Left err
    _         <> Left  err = Left err
    Right x   <> Right y   = Right (x <> y)
