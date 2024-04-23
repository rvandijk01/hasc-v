module Core where

import HASCVTypes

-- placeholder
main :: Int -> Int -> Int
main = (+)

updatePC :: Register -> Register
updatePC = (+ 1)
