module Main where

import qualified Data.List                     as List
import           Lib

add :: Integer -> Integer -> Integer
add x y = x + y

expr :: String
expr = "(\\x.x+x)(1)(2)(\\x.(\\y.x+y))(\\x.(\\y.(\\z.x+y+z)))"

data Term = Variable | Abstraction Term | Application Term Term

data Block = BlockText String | SubBlocks [Block]
instance Show Block where
    show (BlockText a) = a
    show (SubBlocks a) = show a

type IndexRange = (Int, Int)

bgnBlockChar :: Char
bgnBlockChar = '('
endBlockChar :: Char
endBlockChar = ')'

getBlock :: (Int, String, [String]) -> Char -> (Int, String, [String])
getBlock (d, expr, bs) c
    | c == bgnBlockChar && d == 0 && expr == "" = (d + 1, expr, bs)
    | c == bgnBlockChar && d == 0 = (d + 1, "", bs ++ [expr])
    | c == bgnBlockChar           = (d + 1, expr ++ [c], bs)
    | c == endBlockChar && d == 1 = (d - 1, "", bs ++ [expr])
    | c == endBlockChar           = (d - 1, expr ++ [c], bs)
    | otherwise                   = (d, expr ++ [c], bs)

getBlocks :: String -> [Block]
getBlocks text = map getSubBlocks bs
  where
    getSubBlocks :: String -> Block
    getSubBlocks b = case List.find (== bgnBlockChar) b of
        Just _  -> SubBlocks (getBlocks b)
        Nothing -> BlockText b
    (_, _, bs) = foldl getBlock (0, "", []) text

printBlocks :: [Block] -> IO ()
printBlocks b = mapM_ printBlock b
  where
    printBlock :: Block -> IO ()
    printBlock b = case b of
        BlockText t  -> putStrLn t
        SubBlocks bs -> printBlocks bs

getData = printBlocks $ getBlocks expr

main :: IO ()
main = getData
