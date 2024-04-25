module Hart where

import HASCVTypes
import Data.Bits (shiftL)
import qualified Data.IntMap.Strict as IM


-- | Function that steps through the current instruction and returns the new state of the hart
processInstr :: Hart -> Hart
processInstr hart = case instr of
    Nop             -> hart { pc = oldpc + 1 }
    Lui rd imm      -> hart { regFile = IM.adjust (const $ shiftL imm 12) rd oldreg,
                            pc = oldpc + 1}
    Auipc rd imm    -> hart { regFile = IM.adjust (const $ oldpc + (shiftL imm 12)) rd oldreg }
    Addi rd rs imm  -> hart { regFile = IM.adjust (const $ (oldreg IM.! rs) + imm) rd oldreg, 
                            pc = oldpc + 1 }
    _ -> hart
    where
        instr = (instrMem hart) !! (fromEnum $ oldpc)
        oldreg = regFile hart
        oldpc = pc hart
