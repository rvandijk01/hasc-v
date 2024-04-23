module HASCVTypes where

import Data.Bits

type Register = Word32

data CoreState = Core { regFile :: [Register]
                      , pc :: Register 
                      } deriving Show