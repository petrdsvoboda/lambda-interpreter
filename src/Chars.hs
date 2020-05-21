module Chars where

cBegin :: Char
cBegin = '('
cEnd :: Char
cEnd = ')'

cLambda :: Char
cLambda = '\\'
cDot :: Char
cDot = '.'

isChar :: Char -> Char -> Bool
isChar = (==)

isCBegin = isChar cBegin
isCEnd = isChar cEnd
isCLambda = isChar cLambda
isCDot = isChar cDot

isCSubBlock c = isCBegin c && isCLambda c
