module Hart where

import HASCVTypes
import Data.Bits
import Data.Word (Word32)
import Data.Int (Int32)
import qualified Data.IntMap.Strict as IM

setRegister :: Register -> RegIdx -> RegFile -> RegFile
setRegister val rd regbank = IM.adjust (const val) rd regbank

-- | sign-extend first n least significant bits of word 
-- use fromIntegral to go from unsigned representation (Word32) to signed (Int32) and back
sext :: Int -> Word32 -> Word32
sext n word = fromIntegral sExt
    where
        sExt = shiftR (shiftL sWord $ 32-n) $ 32-n
        sWord = (fromIntegral word) :: Int32

-- | zero-extend first n least significant bits of word
zext :: Int -> Word32 -> Word32
zext n word = shiftR (shiftL word $ 32-n) $ 32-n

-- | Function that steps through the current instruction and returns the new state of the hart
processInstr :: Hart -> Hart
processInstr hart = (case instr of
    Nop             -> hart
    Lui rd imm      -> hart { regFile = setRegister (shiftL imm 12) rd regf }
    Auipc rd imm    -> hart { regFile = setRegister (oldpc + (shiftL imm 12)) rd regf }
    Addi rd rs1 imm -> hart { regFile = setRegister ((reg rs1) + imm) rd regf }
    -- TODO: find better solution for signed comparison, this does not work for signed yet (sext does work)
    Slti rd rs1 imm -> hart { regFile = setRegister (if (reg rs1) < (sext 12 imm) then 1 else 0 ) rd regf }
    Sltiu rd rs1 imm-> hart { regFile = setRegister (if (reg rs1) < (sext 12 imm) then 1 else 0 ) rd regf }
    Xori rd rs1 imm -> hart { regFile = setRegister ((reg rs1) `xor` (sext 12 imm)) rd regf }
    Ori rd rs1 imm  -> hart { regFile = setRegister ((reg rs1) .|. (sext 12 imm)) rd regf }
    Andi rd rs1 imm -> hart { regFile = setRegister ((reg rs1) .&. (sext 12 imm)) rd regf }

    -- catch invalid/unimplemented instructions by skipping them
    _               -> hart
    ) { pc = oldpc + 1 }
    where
        instr = (instrMem hart) !! (fromEnum $ oldpc)
        reg = (regf IM.!)
        regf = regFile hart
        oldpc = pc hart


-- for quick testing in ghci
testprog :: Prog
testprog = [Addi 2 2 10, Addi 2 2 10]

testhart :: Hart
testhart = initHart 10 testprog

runN :: Int -> Hart -> Hart
runN n = foldr (.) id $ replicate n processInstr