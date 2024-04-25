module Hart where

import HASCVTypes
import Data.Bits (shiftL)
import qualified Data.IntMap.Strict as IM

setRegister :: Register -> RegIdx -> RegFile -> RegFile
setRegister val rd regbank = IM.adjust (const val) rd regbank

-- | sign-extend first n least significant bits of word
sext :: Int -> Word32 -> Word32
sext n word = 

-- | Function that steps through the current instruction and returns the new state of the hart
processInstr :: Hart -> Hart
processInstr hart = (case instr of
    Nop             -> hart
    Lui rd imm      -> hart { regFile = setRegister (shiftL imm 12) rd reg }
    Auipc rd imm    -> hart { regFile = setRegister (oldpc + (shiftL imm 12)) rd reg }
    Addi rd rs1 imm -> hart { regFile = setRegister ((reg IM.! rs1) + imm) rd reg }
    --Slti rd rs1 imm -> hart { regFile = setRegister (if reg IM.! rs1 ) }

    -- catch invalid/unimplemented instructions by skipping them
    _               -> hart
    ) { pc = oldpc + 1 }
    where
        instr = (instrMem hart) !! (fromEnum $ oldpc)
        reg = regFile hart
        oldpc = pc hart


-- for quick testing in ghci
testprog :: Prog
testprog = [Addi 2 2 10, Addi 2 2 10]

testhart :: Hart
testhart = initHart 10 testprog

runN :: Int -> Hart -> Hart
runN n = foldr (.) id $ replicate n processInstr