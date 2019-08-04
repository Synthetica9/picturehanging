{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Draw where


import           Prelude                      hiding (Either (..))

import           Diagrams.Angle               (turn, (@@))
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude
import           Diagrams.Transform           (Transformation, transform)
import           Diagrams.TwoD                (rotateBy, translate, unitX, yDir)
import           Diagrams.TwoD.Arc            (arc)
import           Diagrams.TwoD.Transform


import           Hanging

diagram :: Int -> Diagram B
diagram nails = let

  dpi = 600

  instructions = simplifyDraw $ drawInstructions $ hanging nails

  -- unit :: Double
  -- unit = 1.0

  nailSize = 1.0

  nailGap = 20 * nailSize
  innerGap = 3 * nailSize
  ropeWidth = nailSize / 5
  ropeSpace = nailGap / 2
  ropeGap = ropeSpace / intToDouble (length instructions)

  intToDouble :: Int -> Double
  intToDouble = fromIntegral

  horMult :: Horizontal -> Double
  horMult Left  = -1
  horMult Right =  1

  pinLocation :: Pin -> Double
  pinLocation n = nailGap + (nailSize + nailGap) * intToDouble n

  crossingPoints :: [Double]
  crossingPoints = (\(pin, hor) -> horMult hor * ropeGap + pinLocation pin) <$> instructions

  unitArcs = cycle $ [id, rotateBy (1/2)] <*> arc yDir ((1/2) @@ turn)
  -- unitArcs = _

  transformations = let
      pairs :: [(Double, Double)]
      pairs = zip crossingPoints $ tail crossingPoints
      f (x, y) = translation (((x + y) / 2) *^ unitX) <> scaling (abs $ x - y)
    in f <$> pairs

  arcs = zipWith transform transformations unitArcs

  in mconcat arcs
