module Hart (processInstr)
-- | This module is responsible for simulating a single RV32I+ Hardware Thread (hart)
where

import Types
import Data.Bits (shiftR, shiftL, xor, (.|.), (.&.))
import Data.Word (Word32, Word8)
import Data.Int (Int32)
import qualified Data.IntMap.Strict as IM

-- | Compute next register file state from old register file, dest. register and computed value
setRegister :: Register -> RegIdx -> RegFile -> RegFile
setRegister val rd regbank = IM.adjust (const val) rd regbank

-- | sign-extend first n least significant bits of word 
-- use fromIntegral to go from unsigned representation (Word32) to signed (Int32) and back
sext :: Int -> Word32 -> Word32
sext n word = fromIntegral sExt
    where
        sExt = sWord `shiftL` (32-n) `shiftR` (32-n)
        sWord = (fromIntegral word) :: Int32

-- | zero-extend first n least significant bits of word
zext :: Int -> Word32 -> Word32
zext n word = shiftR (shiftL word $ 32-n) $ 32-n

-- | 5-bit unsigned to Int for shift instructions
shamt :: Word32 -> Int
shamt = fromIntegral . (zext 5)

-- | comparison between two words when both are interpreted as signed
-- x < y ? 1 : 0
signedcmp :: Word32 -> Word32 -> Word32
signedcmp x y = if (fromIntegral x :: Int32) < (fromIntegral y :: Int32) then 1 else 0

-- | loads 1-4 bytes starting from specified address -> little endian
getbytes :: Int     -- ^ Number of bytes to read (1-4)
         -> Memory  -- ^ Memory bank to read from
         -> Word32  -- ^ Address of first byte (for now: index in memory array)
         -> Word32  -- ^ The value read
getbytes n mem addr = fromIntegral 
                        $ foldr (\(a :: Word32) b -> (shiftL b 8) + a) 0 
                        $ (drop (fromIntegral addr) . take n) 
                        $ map fromIntegral mem

setBytes :: Memory  -- ^ Memory bank to write to
         -> Word32  -- ^ Address of first byte (for now: index in memory array)
         -> [Word8] -- ^ Data to write
         -> Memory  -- ^ New state of the memory bank
setBytes mem addr wdata = fst ++ wdata ++ lst
    where
        fst = take intaddr mem
        lst = drop (intaddr + length wdata) mem
        intaddr = (fromIntegral addr) :: Int

-- | Transforms a 32-bit word to an array of bytes for memory
word2bytes :: Word32 -> [Word8]
word2bytes w = map fromIntegral [shiftR w n | n <- [0, 8, 16, 24]]

-- | Function that steps through the current instruction and returns the new state of the hart
processInstr :: Hart -> Hart
processInstr hart = (case instr of    
    -- [I] immediate instructions
    Lui rd imm      -> hart { regFile = setRegister (shiftL imm 12) rd regf }
    Auipc rd imm    -> hart { regFile = setRegister (oldpc + (shiftL imm 12)) rd regf }
    Addi rd rs1 imm -> hart { regFile = setRegister ((reg rs1) + imm) rd regf }
    Slti rd rs1 imm -> hart { regFile = setRegister (reg rs1 `signedcmp` sext 12 imm) rd regf }
    Sltiu rd rs1 imm-> hart { regFile = setRegister (if (reg rs1) < (sext 12 imm) then 1 else 0) rd regf }
    Xori rd rs1 imm -> hart { regFile = setRegister ((reg rs1) `xor` (sext 12 imm)) rd regf }
    Ori rd rs1 imm  -> hart { regFile = setRegister ((reg rs1) .|. (sext 12 imm)) rd regf }
    Andi rd rs1 imm -> hart { regFile = setRegister ((reg rs1) .&. (sext 12 imm)) rd regf }
    Slli rd rs1 imm -> hart { regFile = setRegister (shiftL (reg rs1) (shamt imm)) rd regf }
    Srli rd rs1 imm -> hart { regFile = setRegister (shiftR (reg rs1) (shamt imm)) rd regf }
    Srai rd rs1 imm -> hart { regFile = setRegister (sext (32 - (shamt imm)) $ shiftR (reg rs1) (shamt imm)) rd regf }
    
    -- [I] non-immediate instructions
    Add rd rs1 rs2  -> hart { regFile = setRegister ((reg rs1) + (reg rs2)) rd regf }
    Sub rd rs1 rs2  -> hart { regFile = setRegister ((reg rs1) + (reg rs2)) rd regf }
    Sll rd rs1 rs2  -> hart { regFile = setRegister (shiftL (reg rs1) (shamt $ reg rs2)) rd regf }
    Slt rd rs1 rs2  -> hart { regFile = setRegister (reg rs1 `signedcmp` reg rs2) rd regf}
    Sltu rd rs1 rs2 -> hart { regFile = setRegister (if (reg rs1) < (reg rs2) then 1 else 0) rd regf }
    Xor rd rs1 rs2  -> hart { regFile = setRegister ((reg rs1) `xor` (reg rs2)) rd regf }
    Srl rd rs1 rs2  -> hart { regFile = setRegister (shiftR (reg rs1) (shamt $ reg rs2)) rd regf }
    Sra rd rs1 rs2  -> hart { regFile = setRegister (sext (32 - (shamt $ reg rs2)) $ shiftR (reg rs1) (shamt $ reg rs2)) rd regf }
    Or rd rs1 rs2   -> hart { regFile = setRegister ((reg rs1) .|. (reg rs2)) rd regf }
    And rd rs1 rs2  -> hart { regFile = setRegister ((reg rs1) .&. (reg rs2)) rd regf }

    -- [I] load/store instructions
    Lb rd off rs1   -> hart { regFile = setRegister (sext 8 $ getbytes 1 lmem $ reg rs1 + sext 12 off) rd regf }
    Lh rd off rs1   -> hart { regFile = setRegister (sext 16 $ getbytes 2 lmem $ reg rs1 + sext 12 off) rd regf }
    Lw rd off rs1   -> hart { regFile = setRegister (getbytes 4 lmem $ reg rs1 + sext 12 off) rd regf }
    Lbu rd off rs1  -> hart { regFile = setRegister (getbytes 1 lmem $ reg rs1 + sext 12 off) rd regf }
    Lhu rd off rs1  -> hart { regFile = setRegister (getbytes 2 lmem $ reg rs1 + sext 12 off) rd regf }
    Sb rs2 off rs1  -> hart { localMem = setBytes lmem (reg rs1 + sext 12 off) (take 1 $ word2bytes $ reg rs2)}
    Sh rs2 off rs1  -> hart { localMem = setBytes lmem (reg rs1 + sext 12 off) (take 2 $ word2bytes $ reg rs2)}
    Sw rs2 off rs1  -> hart { localMem = setBytes lmem (reg rs1 + sext 12 off) (take 4 $ word2bytes $ reg rs2)}

    -- [I] jump/branch instructions (TODO)

    -- [M] integer HW multiply/divide instructions (TODO)


    -- catch invalid/unimplemented/nop instructions and skip
    _               -> hart
    ) { pc = oldpc + 1 }
    where
        instr = (instrMem hart) !! (fromEnum $ oldpc)
        reg = (regf IM.!)
        regf = regFile hart
        lmem = localMem hart
        oldpc = pc hart


-- for quick testing in ghci
-- this should be tested much more extensively before saying RV32IM works
testprog :: Prog
testprog = [Addi 2 2 10, Addi 2 2 255, Sw 2 0 0]

testhart :: Hart
testhart = initHart 10 testprog

runN :: Int -> Hart -> Hart
runN n = foldr (.) id $ replicate n processInstr