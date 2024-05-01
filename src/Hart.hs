module Hart (processInstr)
-- | This module is responsible for simulating a single RV32I+ Hardware Thread (hart)
where

import Types
import Data.Bits (shiftR, shiftL, xor, (.|.), (.&.))
import Data.Word (Word64, Word32, Word8)
import Data.Int (Int64, Int32)
import qualified Data.IntMap.Strict as IM
import Debug.Trace (trace)

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

-- | Does the instruction affect the PC?
affectsPC :: Instr -> Bool
affectsPC (Jal _ _) = True; affectsPC (Jalr _ _ _) = True; affectsPC (Beq _ _ _) = True
affectsPC (Bne _ _ _) = True; affectsPC (Blt _ _ _) = True; affectsPC (Bge _ _ _) = True
affectsPC (Bltu _ _ _) = True; affectsPC (Bgeu _ _ _) = True; affectsPC _ = False

-- | simulation of 32x32 unsigned hardware multiplier
hwmulu :: Word32 -> Word32 -> Word64
hwmulu a b = ((fromIntegral a) :: Word64) * ((fromIntegral b) :: Word64)

-- | simulation of 32x32 signed hardware multiplier
hwmuls :: Word32 -> Word32 -> Word64
hwmuls a b = fromIntegral $ ((fromIntegral a) :: Int64) * ((fromIntegral b) :: Int64) :: Word64

hwdivu :: Word32 -> Word32 -> Word32
hwdivu = div

hwdivs :: Word32 -> Word32 -> Word32
hwdivs a b = fromIntegral $ ((fromIntegral a) :: Int32) `div` ((fromIntegral b) :: Int32)

hwremu :: Word32 -> Word32 -> Word32
hwremu = mod

hwrems :: Word32 -> Word32 -> Word32
hwrems a b = fromIntegral $ ((fromIntegral a) :: Int32) `mod` ((fromIntegral b) :: Int32)


-- | Function that steps through the current instruction and returns the new state of the hart
processInstr :: Hart -> Hart
processInstr hart = case instr of    
    -- [I] immediate instructions
    Lui rd imm      -> hart { regFile = set rd $ imm `shiftL` 12,                                               pc = oldpc+1 }
    Auipc rd imm    -> hart { regFile = set rd $ oldpc + (imm `shiftL` 12),                                     pc = oldpc+1 }
    Addi rd rs1 imm -> hart { regFile = set rd $ (reg rs1) + imm,                                               pc = oldpc+1 }
    Slti rd rs1 imm -> hart { regFile = set rd $ reg rs1 `signedcmp` sext 12 imm,                               pc = oldpc+1 }
    Sltiu rd rs1 imm-> hart { regFile = set rd $ if (reg rs1) < (sext 12 imm) then 1 else 0,                    pc = oldpc+1 }
    Xori rd rs1 imm -> hart { regFile = set rd $ (reg rs1) `xor` (sext 12 imm),                                 pc = oldpc+1 }
    Ori rd rs1 imm  -> hart { regFile = set rd $ (reg rs1) .|. (sext 12 imm),                                   pc = oldpc+1 }
    Andi rd rs1 imm -> hart { regFile = set rd $ (reg rs1) .&. (sext 12 imm),                                   pc = oldpc+1 }
    Slli rd rs1 imm -> hart { regFile = set rd $ (reg rs1) `shiftL` (shamt imm),                                pc = oldpc+1 }
    Srli rd rs1 imm -> hart { regFile = set rd $ (reg rs1) `shiftR` (shamt imm),                                pc = oldpc+1 }
    Srai rd rs1 imm -> hart { regFile = set rd $ sext (32 - shamt imm) $ (reg rs1) `shiftR` shamt imm,          pc = oldpc+1 }
    
    -- [I] non-immediate instructions
    Add rd rs1 rs2  -> hart { regFile = set rd $ reg rs1 + reg rs2,                                             pc = oldpc+1 }
    Sub rd rs1 rs2  -> hart { regFile = set rd $ reg rs1 + reg rs2,                                             pc = oldpc+1 }
    Sll rd rs1 rs2  -> hart { regFile = set rd $ reg rs1 `shiftL` (shamt $ reg rs2),                            pc = oldpc+1 }
    Slt rd rs1 rs2  -> hart { regFile = set rd $ reg rs1 `signedcmp` reg rs2,                                   pc = oldpc+1 }
    Sltu rd rs1 rs2 -> hart { regFile = set rd $ if (reg rs1) < (reg rs2) then 1 else 0,                        pc = oldpc+1 }
    Xor rd rs1 rs2  -> hart { regFile = set rd $ reg rs1 `xor` reg rs2,                                         pc = oldpc+1 }
    Srl rd rs1 rs2  -> hart { regFile = set rd $ reg rs1 `shiftR` (shamt $ reg rs2),                            pc = oldpc+1 }
    Sra rd rs1 rs2  -> hart { regFile = set rd $ sext (32 - (shamt $ reg rs2)) $ (reg rs1) `shiftR` (shamt $ reg rs2), pc = oldpc+1 }
    Or rd rs1 rs2   -> hart { regFile = set rd $ reg rs1 .|. reg rs2,                                           pc = oldpc+1 }
    And rd rs1 rs2  -> hart { regFile = set rd $ reg rs1 .&. reg rs2,                                           pc = oldpc+1 }

    -- [I] load/store instructions
    Lb rd off rs1   -> hart { regFile = set rd $ sext 8 $ getbytes 1 lmem $ reg rs1 + sext 12 off,              pc = oldpc+1 }
    Lh rd off rs1   -> hart { regFile = set rd $ sext 16 $ getbytes 2 lmem $ reg rs1 + sext 12 off,             pc = oldpc+1 }
    Lw rd off rs1   -> hart { regFile = set rd $ getbytes 4 lmem $ reg rs1 + sext 12 off,                       pc = oldpc+1 }
    Lbu rd off rs1  -> hart { regFile = set rd $ getbytes 1 lmem $ reg rs1 + sext 12 off,                       pc = oldpc+1 }
    Lhu rd off rs1  -> hart { regFile = set rd $ getbytes 2 lmem $ reg rs1 + sext 12 off,                       pc = oldpc+1 }
    Sb rs2 off rs1  -> hart { localMem = setBytes lmem (reg rs1 + sext 12 off) (take 1 $ word2bytes $ reg rs2), pc = oldpc+1 }
    Sh rs2 off rs1  -> hart { localMem = setBytes lmem (reg rs1 + sext 12 off) (take 2 $ word2bytes $ reg rs2), pc = oldpc+1 }
    Sw rs2 off rs1  -> hart { localMem = setBytes lmem (reg rs1 + sext 12 off) (take 4 $ word2bytes $ reg rs2), pc = oldpc+1 }

    -- [I] jump/branch instructions
    -- Note: proof-of-concept implementation, some details to be worked out, not fully working yet
    Jal rd off      -> hart { regFile = set rd $ oldpc + 1, 
                              pc = oldpc + sext 20 off }     -- -1 from pc to account for pc+1 at the end
    Jalr rd rs1 off -> hart { regFile = set rd $ oldpc + 1,
                              pc = oldpc + reg rs1 + sext 12 off }
    Beq rs1 rs2 off -> hart { pc = if reg rs1 == reg rs2 then oldpc + sext 12 off else oldpc+1 }
    Bne rs1 rs2 off -> hart { pc = if reg rs1 /= reg rs2 then oldpc + sext 12 off else oldpc+1 }
    Blt rs1 rs2 off -> hart { pc = if reg rs1 `signedcmp` reg rs2 /= 0 then oldpc + sext 12 off else oldpc+1 }
    Bge rs1 rs2 off -> hart { pc = if reg rs1 `signedcmp` reg rs2 == 0 then oldpc + sext 12 off else oldpc+1 }
    Bltu rs1 rs2 off-> hart { pc = if reg rs1 < reg rs2 then oldpc + sext 12 off else oldpc+1 }
    Bgeu rs1 rs2 off-> hart { pc = if reg rs1 >= reg rs2 then oldpc + sext 12 off else oldpc+1 }

    -- [M] integer HW multiply/divide instructions (Mulhsu unsure)
    Mul rd rs1 rs2  -> hart { regFile = set rd $ fromIntegral $ (.&.) 0xFFFFFFFF $ reg rs1 `hwmuls` reg rs2,    pc = oldpc+1 }
    Mulh rd rs1 rs2 -> hart { regFile = set rd $ fromIntegral $ (reg rs1 `hwmuls` reg rs2) `shiftR` 32,         pc = oldpc+1 }
    Mulhsu rd rs1 rs2->hart { regFile = set rd $ fromIntegral $ reg rs1 `hwmulu` reg rs2,                       pc = oldpc+1 }
    Mulhu rd rs1 rs2-> hart { regFile = set rd $ fromIntegral $ (reg rs1 `hwmulu` reg rs2) `shiftR` 32,         pc = oldpc+1 }
    Div rd rs1 rs2  -> hart { regFile = set rd $ reg rs1 `hwdivs` reg rs2,                                      pc = oldpc+1 }
    Divu rd rs1 rs2 -> hart { regFile = set rd $ reg rs1 `hwdivu` reg rs2,                                      pc = oldpc+1 }
    Rem rd rs1 rs2  -> hart { regFile = set rd $ reg rs1 `hwrems` reg rs2,                                      pc = oldpc+1 }
    Remu rd rs1 rs2 -> hart { regFile = set rd $ reg rs1 `hwremu` reg rs2,                                      pc = oldpc+1 }

    -- Custom instructions
    Sout rs1        -> trace ((++) ("Hart " ++ (show $ hartid hart) ++ " says: ") $ show $ reg rs1) $ hart {    pc = oldpc+1 }

    -- catch invalid/unimplemented/nop instructions and skip
    _               -> hart
    where
        instr = (instrMem hart) !! (fromEnum $ oldpc)
        reg = (regf IM.!)
        set rd val = setRegister val rd regf
        regf = regFile hart
        lmem = localMem hart
        oldpc = pc hart