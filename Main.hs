{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude

import           Draw                         (diagram)

main :: IO ()
main = mainWith $ diagram 2
