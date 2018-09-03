{-# LANGUAGE LambdaCase, OverloadedLists, MultiWayIf #-}
module Enumerable where

import Data.Semigroup
import Data.List
import Control.Monad.Omega
import Control.Applicative
import Unsafe.Coerce
import Data.Function (fix)
import Control.Monad
import Debug.Trace
import Data.Char (digitToInt)

data Desc a
  = Zero a -- Unit
  | One   (a -> a)
  | Two   (a -> a -> a)
  | Three (a -> a -> a -> a)
  | Four  (a -> a -> a -> a -> a)
  | Five  (a -> a -> a -> a -> a -> a)
  -- | Six   (a -> a -> a -> a -> a -> a -> a)
  -- | Seven (a -> a -> a -> a -> a -> a -> a -> a)
  | And (Desc a) (Desc a)

instance Semigroup (Desc a) where (<>) = And



toList :: Desc a -> [Desc a]
toList (And a b) = toList a ++ toList b
toList         x = pure x

enum :: Desc a -> [a]
enum d = runOmega go
  where ds = toList d
        go = each ds >>= \case
          Zero  a -> pure a
          One   f -> fmap f go
          Two   f -> liftA2 f go go
          Three f -> liftA3 f go go go
          Four  f -> liftM4 f go go go go
          Five  f -> liftM5 f go go go go go


-- Dumb bruteforce
index :: Eq a => a -> Desc a -> Maybe Int
index a d = elemIndex a (enum d)

fromIndex :: Desc a -> Int -> a
fromIndex d ix = enum d !! ix

-- Clever

-- Assumes exactly one unit (Zero)
fromIndex' :: Desc a -> Integer -> a
fromIndex' d 0 = head $ enum d
fromIndex' d ix
  | ds <- toList d
  , l  <- fromIntegral $ length ds
  , (x,y) <- locN (l-1) ix
  = case ds !! fromIntegral x of
      One f -> f $ fromIndex' d y
      Two f | (x', y') <- loc' y -> f (fromIndex' d x') (fromIndex' d y')

      Zero _ -> error "Only one unit supported!"
      _      -> error "NOT IMPLEMENTED"


-----


-- Sum of the N first integers
seqSum n = n*(n+1) `div` 2

-- Assumes exactly one unit and infinite elements
loc ix
  | diag   <- ceiling $ (sqrt (8*fromIntegral ix + 1) - 1) / 2
  , s      <- seqSum (diag-1)
  , row    <- ix - s
  , height <- diag + s - ix
  = (row, height)

-- Assumes exactly one unit and N elements
locN n ix
  | seqSum n >= ix = loc ix
  | s      <- seqSum n + 1
  , (d,m)  <- divMod (ix - s) n
  , row    <- m + 1
  , height <- d + n - m
  = (row, height)


-- Assumes no unit and infinite elements
loc' ix
  | diag <- floor $ (sqrt (8*fromIntegral ix + 1) - 1) / 2
  , col  <- ix - seqSum diag
  = (col, diag - col)

-- Assumes no unit and N elements
locN' n ix
  | seqSum (n+1) >= ix = loc' ix
  | diag <- floor $ (sqrt (8*fromIntegral ix + 1) - 1) / 2
  , s <- seqSum n
  , (d,m)  <- divMod (ix - s) n
  = (m,n-m+d)


----


data Math
  = Var
  | NEG Math
  | ADD  Math Math
  | MULT Math Math
  | DIV  Math Math
  deriving (Show, Eq)

math = Zero Var <> One NEG <> Two ADD <> Two MULT <> Two DIV

calc  Var       v = v
calc (NEG  x  ) v = negate $ calc x v
calc (ADD  x y) v = calc x v + calc y v
calc (MULT x y) v = calc x v * calc y v
calc (DIV  x y) v = calc x v `div` calc y v

rpn  Var       = "x"
rpn (NEG x)    = rpn x ++ " neg"
rpn (ADD x y)  = rpn y ++ " " ++ rpn x ++ " +"
rpn (MULT x y) = rpn y ++ " " ++ rpn x ++ " *"
rpn (DIV x y)  = rpn y ++ " " ++ rpn x ++ " /"

