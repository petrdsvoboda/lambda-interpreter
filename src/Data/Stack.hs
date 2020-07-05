{-|
Module      : Data.Stack
Description : Stack data type
Copyright   : (c) Petr Svoboda, 2020
License     : GPL-3
Maintainer  : svobop51@fit.cvut.cz
Stability   : experimental
Portability : POSIX

Simulates stack
-}
module Data.Stack where

import qualified Data.List                     as List

-- | Recursive structure simulating stack
data Stack a = Empty | NonEmpty a (Stack a)
             deriving (Show, Read, Eq)

-- | Empty stack
empty :: Stack a
empty = Empty

-- | Get element from top of stack
-- If stack is empty error with message "Empty stack" will be raised
top :: Stack a -> a
top Empty          = error "Empty stack"
top (NonEmpty a _) = a

-- | Get element from top of stack (if there is some, otherwise return Nothing)
topSafe :: Stack a -> Maybe a
topSafe Empty          = Nothing
topSafe (NonEmpty a _) = Just a

-- | Pop element from top of stack
-- If stack is empty error with message "Empty stack" will be raised
pop :: Stack a -> Stack a
pop Empty          = error "Empty stack"
pop (NonEmpty _ a) = a

-- | Pop element from top of stack (if there is some, otherwise return Nothing)
popSafe :: Stack a -> Maybe (Stack a)
popSafe Empty          = Nothing
popSafe (NonEmpty _ a) = Just a

-- | Push element to top of stack
push :: a -> Stack a -> Stack a
push = NonEmpty

-- | Get number of elements in stack
size :: Num n => Stack a -> n
size Empty          = 0
size (NonEmpty _ a) = 1 + size a

-- | Check if stack is empty
null :: Stack a -> Bool
null Empty = True
null _     = False

-- | Create stack from list
fromList :: [a] -> Stack a
fromList []   = Empty
fromList list = push x (fromList rest)
 where
  x    = List.head list
  rest = List.tail list
