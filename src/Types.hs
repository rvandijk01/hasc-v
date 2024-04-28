module Types
    -- | Algebraic datatype definitions and their "constructors"
    -- Meant to accomodate the RISC-V spec rather than comply with it
    -- for example, types in Haskell cannot represent 20-bit immediate values for AUIPC etc

    -- However, restrictions are enforced using the type system when possible
where

import Data.Word (Word32, Word8)
import Data.Bits (zeroBits)
import qualified Data.IntMap.Strict as IM

-- | Type synonyms to increase clarity in function definitions
type Register = Word32              -- ^ In principle an unsigned 32-bit value, convert to Int32 to interpret signed
type RegFile = IM.IntMap Register   -- ^ (non-lazy) mapping from register index to register
type RegIdx = Int                   -- ^ Register index (0-31)
type Imm = Word32                   -- ^ Immediate value, simulation should enforce amount of bits
type Prog = [Instr]                 -- ^ For now a program is just a list of instructions

-- | https://msyksphinz-self.github.io/riscv-isadoc/
-- For now, the following (RV32I) instruction types are not supported: fence, csr, privileged, interrupt
data Instr =    
            Nop                             -- ^ no operation
            
            -- [I] Arithmetic & Logic instructions with immediate value
            |   Lui     RegIdx Imm              -- ^ x[rd] = sext(imm << 12)
            |   Auipc   RegIdx Imm              -- ^ x[rd] = pc + sext(imm << 12)
            |   Addi    RegIdx RegIdx Imm       -- ^ x[rd] = x[rs1] + sext(imm)
            |   Slti    RegIdx RegIdx Imm       -- ^ x[rd] = x[rs1] < sext(imm)     [SIGNED]
            |   Sltiu   RegIdx RegIdx Imm       -- ^ x[rd] = x[rs1] < sext(imm)     [UNSIGNED]
            |   Xori    RegIdx RegIdx Imm       -- ^ x[rd] = x[rs1] ^ sext(imm)
            |   Ori     RegIdx RegIdx Imm       -- ^ x[rd] = x[rs1] | sext(imm)
            |   Andi    RegIdx RegIdx Imm       -- ^ x[rd] = x[rs1] & sext(imm)
            |   Slli    RegIdx RegIdx Imm       -- ^ x[rd] = x[rs1] << imm          [LOGIC]
            |   Srli    RegIdx RegIdx Imm       -- ^ x[rd] = x[rs1] >> imm          [LOGIC]
            |   Srai    RegIdx RegIdx Imm       -- ^ x[rd] = x[rs1] >> imm          [ARITH]
            
            -- [I] Arithmetic & Logic instructions on registers only
            |   Add     RegIdx RegIdx RegIdx    -- ^ x[rd] = x[rs1] + x[rs2]
            |   Sub     RegIdx RegIdx RegIdx    -- ^ x[rd] = x[rs1] - x[rs2]
            |   Sll     RegIdx RegIdx RegIdx    -- ^ x[rd] = x[rs1] << x[rs2]       [LOGIC]
            |   Slt     RegIdx RegIdx RegIdx    -- ^ x[rd] = x[rs1] < x[rs2]        [SIGNED]
            |   Sltu    RegIdx RegIdx RegIdx    -- ^ x[rd] = x[rs1] < x[rs2]        [UNSIGNED]
            |   Xor     RegIdx RegIdx RegIdx    -- ^ x[rd] = x[rs1] ^ x[rs2]
            |   Srl     RegIdx RegIdx RegIdx    -- ^ x[rd] = x[rs1] >> x[rs2]       [LOGIC]
            |   Sra     RegIdx RegIdx RegIdx    -- ^ x[rd] = x[rs1] >> x[rs2]       [ARITH]
            |   Or      RegIdx RegIdx RegIdx    -- ^ x[rd] = x[rs1] | x[rs2]
            |   And     RegIdx RegIdx RegIdx    -- ^ x[rd] = x[rs1] & x[rs2]
            
            -- [I] load/store instructions
            -- TODO: implement lbu-sw
            |   Lb      RegIdx Imm RegIdx       -- ^ x[rd] = sext(mem[x[rs1] + sext(offset)][7:0])
            |   Lh      RegIdx Imm RegIdx       -- ^ x[rd] = sext(mem[x[rs1] + sext(offset)][15:0])
            |   Lw      RegIdx Imm RegIdx       -- ^ x[rd] = sext(mem[x[rs1] + sext(offset)][31:0])
            |   Lbu     RegIdx Imm RegIdx       -- ^ x[rd] = mem[x[rs1] + sext(offset)][7:0]
            |   Lhu     RegIdx Imm RegIdx       -- ^ x[rd] = mem[x[rs1] + sext(offset)][15:0]
            |   Sb      RegIdx Imm RegIdx       -- ^ mem[x[rs1] + sext(offset)] = x[rs2][7:0]
            |   Sh      RegIdx Imm RegIdx       -- ^ mem[x[rs1] + sext(offset)] = x[rs2][15:0]
            |   Sw      RegIdx Imm RegIdx       -- ^ mem[x[rs1] + sext(offset)] = x[rs2][31:0]

            -- [I] jump/branch instructions
            -- TODO: implement all
            |   Jal     RegIdx Imm              -- ^ x[rd] = ++pc; pc += sext(offset)
            |   Jalr    RegIdx RegIdx Imm       -- ^ t = pc+4; pc = (x[rs1]+sext(offset))&∼1; x[rd] = t
            |   Beq     RegIdx RegIdx Imm       -- ^ (x[rs1] == x[rs2]) --> pc += sext(offset)
            |   Bne     RegIdx RegIdx Imm       -- ^ (x[rs1] != x[rs2]) --> pc += sext(offset)
            |   Blt     RegIdx RegIdx Imm       -- ^ (x[rs1] < x[rs2]) --> pc += sext(offset)           [SIGNED]
            |   Bge     RegIdx RegIdx Imm       -- ^ (x[rs1] >= x[rs2]) --> pc += sext(offset)          [SIGNED]
            |   Bltu    RegIdx RegIdx Imm       -- ^ (x[rs1] < x[rs2]) --> pc += sext(offset)           [UNSIGNED]
            |   Bgeu    RegIdx RegIdx Imm       -- ^ (x[rs1] >= x[rs2]) --> pc += sext(offset)          [UNSIGNED]

            -- [M] integer multiplier instructions as defined in the "M" extension
            -- TODO: implement all
            |   Mul     RegIdx RegIdx RegIdx    -- ^ x[rd] = (signed(x[rs1]) * signed(x[rs2]))[31:0]
            |   Mulh    RegIdx RegIdx RegIdx    -- ^ x[rd] = (signed(x[rs1]) * signed(x[rs2]))[63:32]
            |   Mulhsu  RegIdx RegIdx RegIdx    -- ^ x[rd] = (signed(x[rs1]) * unsigned(x[rs2]))[63:32]
            |   Mulhu   RegIdx RegIdx RegIdx    -- ^ x[rd] = (unsigned(x[rs1]) * unsigned(x[rs2]))[63:32]
            |   Div     RegIdx RegIdx RegIdx    -- ^ x[rd] = signed(x[rs1]) / signed(x[rs2])
            |   Divu    RegIdx RegIdx RegIdx    -- ^ x[rd] = unsigned(x[rs1]) / unsigned(x[rs2])
            |   Rem     RegIdx RegIdx RegIdx    -- ^ x[rd] = signed(x[rs1]) % signed(x[rs2])
            |   Remu    RegIdx RegIdx RegIdx    -- ^ x[rd] = unsigned(x[rs1]) % unsigned(x[rs2])
            
            -- insert more instructions here

            deriving Show
            
-- | Little endian unsigned byte array
type Memory = [Word8]

-- | Hardware thread (hart) with local memory
data Hart = Hart { 
    regFile     :: RegFile,
    pc          :: Register,
    localMem    :: Memory,
    instrMem    :: [Instr]
    } deriving Show

-- | CPU with 1 or more harts and shared memory
data CPU = CPU { 
    harts       :: [Hart],
    sharedMem   :: Memory
    } deriving Show



-- | Initializes and configures a new hardware thread (hart)
initHart :: Int     -- ^ Local memory size for this hart
         -> [Instr] -- ^ The instruction memory of this hart
         -> Hart    -- ^ The resulting hart structure
initHart lMemSize prog = Hart { 
    regFile = IM.fromList [(idx, zeroBits :: Word32) | idx <- [0..31]],
    pc = zeroBits,
    localMem = replicate lMemSize zeroBits,
    instrMem = prog
    }

-- | Initializes and configures a new CPU (1 or more harts with shared memory)
initCPU :: Int      -- ^ Local memory size per hart
        -> Int      -- ^ Shared memory size
        -> [Prog]-- ^ For each given program, a hart will be created to run that program locally
        -> CPU      -- ^ The resulting CPU structure
initCPU lMemSize shMemSize progs = CPU { 
    harts = initHart lMemSize <$> progs,
    sharedMem = replicate shMemSize zeroBits
    }