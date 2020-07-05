{-|
Module      : Parser
Description : Conversion from sting to tokens
Copyright   : (c) Petr Svoboda, 2020
License     : GPL-3
Maintainer  : svobop51@fit.cvut.cz
Stability   : experimental
Portability : POSIX

Manages conversion from tokens to representation of term in lambda calculus
-}
module Parser
  ( parse
  , parseVar
  , parseStatement
  , fromString
  , fromStringChecked
  , smartEq
  , toString
  , append
  )
where

import qualified Data.List                     as List
import           Data.List.Split
import qualified Data.Char                     as Char
import qualified Data.Stack                    as Stack
import           Types
import           Lexer
import           EvaluatorHelpers


-- | Appends to Application preserving structure of appended term
-- So basically it won't merge applications like the <> operator does
append :: Term -> Term -> Term
append t1 t2 = case t1 of
  Application ts -> Application (ts ++ [appended])
  Empty          -> appended
  _              -> Application [t1, appended]
 where
  appended = case t2 of
    Application _ -> t2
    Abstraction _ -> t2
    _             -> Application [t2]

-- | Identifies variable or macro
-- Variable must have first letter lowercase, the rest are macros
parseVar :: String -> Term
parseVar text = if Char.isLower $ head text then Variable text else Macro text

-- | Empty item for stack which parser uses
emptyItem :: StackItem
emptyItem = StackItem { isFn = False, inFn = False, fnVar = [], term = Empty }

-- | Parses token and manipulates stack
-- Expands and consolidates stack based on separator tokens
-- Otherwise creates term on the current level
parseToken :: Stack.Stack StackItem -> Token -> Stack.Stack StackItem
parseToken stack token = expand . analyze $ consolidate stack
 where
  -- | Push new item on stack if begin separator
  expand :: Stack.Stack StackItem -> Stack.Stack StackItem
  expand stack | token == Separator Begin = Stack.push emptyItem stack
               | otherwise                = stack
  -- | Consolidate 2 top items on stack if end separator
  consolidate :: Stack.Stack StackItem -> Stack.Stack StackItem
  consolidate stack
    | token == Separator End = Stack.push next (Stack.pop (Stack.pop stack))
    | otherwise              = stack
   where
    StackItem { fnVar = fnVar, term = term } = Stack.top stack
    prev@StackItem { term = termPrev }       = Stack.top (Stack.pop stack)
    -- | Term is not a function if there are no fn arguments
    notFn = List.null fnVar
    -- | merge prev stack item with current (application)
    next  = case term == Empty of
      True  -> prev
      False -> case notFn of
        True  -> prev { term = append termPrev term }
        False -> prev { term = append termPrev $ Abstraction (fnVar, term) }
  -- | Checks phase of item parsing
  -- 1) if term is fn
  -- 2) identifying arguments of fn
  -- 3) ending fn identification
  -- 4) identifying variables and macros
  analyze :: Stack.Stack StackItem -> Stack.Stack StackItem
  analyze stack = Stack.push stackItem (Stack.pop stack)
   where
    curr@StackItem { isFn = isFn, inFn = inFn, fnVar = fnVar, term = term } =
      Stack.top stack
    stackItem = case token of
      Identifier x -> case inFn of
        True  -> curr { fnVar = fnVar ++ [x] }
        False -> curr { term = term <> var }
        where var = parseVar x
      Keyword key -> case key of
        Fn    -> curr { isFn = True, inFn = True }
        EndFn -> curr { inFn = False }
      _ -> curr

-- | Converts tokens into term
-- uses stack to save depth context
parseStatement :: [Token] -> Term
parseStatement tokens = term
 where
  stack = foldl parseToken (Stack.fromList [emptyItem]) tokens
  StackItem { term = term } = Stack.top stack

-- | Converts tokens into expression
-- expression keeps track of parsed term
-- and macro which it is assigned to if tokens contain assignment
parse :: [Token] -> Expr
parse tokens = (term, assignTo)
 where
  -- | second token is assignement keyword
  isAssignment = length tokens > 1 && (tokens !! 1 == (Keyword Assign))
  assignTo     = if not isAssignment
    then Nothing
    else case head tokens of
      Identifier x -> Just x
      _            -> Nothing
  -- | gets tokens to parse
  tokensStatement = if isAssignment then drop 2 tokens else tokens
  term            = parseStatement tokensStatement

-- | Converts string to term (uses tokenize)
fromString :: String -> Term
fromString = parseStatement . tokenize

-- | Converts string to term (uses tokenize)
-- additionally it validates okens before parsing and returns possible errors
fromStringChecked :: String -> Either String Expr
fromStringChecked string = case validate tokens of
  Just err -> Left err
  Nothing  -> Right $ parse tokens
  where tokens = tokenize string

-- | Provides equality check used in the final macro lookup
-- It ignores variable names and compares structure instead
smartEq :: [String] -> Term -> Term -> Bool
smartEq used abs1@(Abstraction (vs1, t1)) abs2@(Abstraction (vs2, t2)) =
  basic || complex
 where
  -- | Normal equality
  basic       = abs1 == abs2
  varLen      = length vs1 == length vs2
  -- | Find specially marked vars
  markedSplit = map (splitOn "_.") (used ++ vs1 ++ vs2)
  markedNums =
    map (\x -> read (x !! 1) :: Int) (filter (\x -> length x > 1) markedSplit)
  -- | Get the highest number used for special vars
  markedHighest = if null markedNums then 0 else maximum markedNums
  -- | New special vars to be used
  newVs =
    [ "_." ++ show x | x <- [markedHighest .. (length vs1 + markedHighest)] ]
  normalizeSingle :: Term -> (String, String) -> Term
  normalizeSingle t (prev, new) = replace prev (Variable new) t
  -- | Convert term to use special vars
  normalize :: Term -> [String] -> Term
  normalize t vs = foldl normalizeSingle t (zip vs newVs)
  n1      = normalize t1 vs1
  n2      = normalize t2 vs2
  -- | Compare terms with normalized vars
  complex = varLen && smartEq (used ++ newVs) n1 n2
smartEq used (Application (t1 : ts1)) (Application (t2 : ts2)) =
  smartEq used t1 t2 && smartEq used (Application ts1) (Application ts2)
smartEq used (Application [t1]) t2                 = smartEq used t1 t2
smartEq used t1                 (Application [t2]) = smartEq used t1 t2
smartEq used t1                 t2                 = t1 == t2

-- | Converts term to string
-- if term is equal to existing macro, reurns macro
toString :: MacroHeap -> Term -> String
toString macros term =
  case List.find (\(_, _, t) -> smartEq [] term t) macros of
    Just (id, _, _) -> id
    Nothing         -> show term
