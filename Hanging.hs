{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE TupleSections  #-}

module Hanging where

import           Control.Arrow (second)
import           Prelude       hiding (Either (..))

type Pin = Int
data Rotation = Clock | Counter deriving (Show, Eq)
data Horizontal = Left | Right deriving (Show, Eq)
type HangingSequence = [(Pin, Rotation)]
type DrawSequence = [(Pin, Horizontal)]

hanging :: Int -> HangingSequence
hanging n | n <= 0 = []
hanging 1 = [(0, Clock)] -- Just hanging it like you would if you weren't a massive nerd
hanging (n + 1) = let xs = hanging n in xs ++ [(n, Counter)] ++ contra xs ++ [(n, Clock)]

flipRotation :: Rotation -> Rotation
flipRotation Clock   = Counter
flipRotation Counter = Clock

contra :: HangingSequence -> HangingSequence
contra = reverse . fmap (second flipRotation)

hang :: Rotation -> [Horizontal]
hang Clock   = [Left, Right]
hang Counter = [Right, Left]

drawInstructions :: HangingSequence -> DrawSequence
drawInstructions xs = xs >>= \(n, rot) -> (n,) <$> hang rot

simplifyDraw :: DrawSequence -> DrawSequence
simplifyDraw []                             = []
simplifyDraw ((n, Right) : (m, Left)  : xs) | n + 1 == m = simplifyDraw xs
simplifyDraw ((n, Left)  : (m, Right) : xs) | n == m + 1 = simplifyDraw xs
simplifyDraw (x : xs)                       = x : simplifyDraw xs
