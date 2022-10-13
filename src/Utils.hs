module Utils where

import Data.List ( isPrefixOf, foldl',isPrefixOf )
import Data.Char (isSpace)

data Variable = Variable {
                  -- | 
                  name :: String,
                  -- | 
                  vtype :: String,
                  -- | 
                  value :: Int }
                deriving Show

type Env = [Variable]
type ArrayType = [Int]

dot :: ArrayType -> ArrayType -> Int
dot x y = sum $ zipWith (*) x y


addArr :: ArrayType -> ArrayType -> ArrayType
addArr = zipWith (+)


mulArr :: ArrayType -> ArrayType -> ArrayType
mulArr = zipWith (*)

sumAllElemArr :: ArrayType -> Int
sumAllElemArr = foldl' (+) 0

mulAllElemArr :: ArrayType -> Int
mulAllElemArr = foldl' (*) 1

toString :: Bool -> String
toString x = if x then "True" else "False"

trim :: String -> String
trim = filter (not.isSpace)