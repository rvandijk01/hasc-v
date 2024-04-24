module Hart where

import HASCVTypes

updatePC :: Register -> Register
updatePC = (+ 1)

processInstr :: Hart -> Instr -> Hart
processInstr = undefined