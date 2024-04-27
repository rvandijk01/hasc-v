module Types
    -- | Algebraic datatype definitions and their "constructors"
    -- Meant to accomodate the RISC-V spec rather than comply with it
    -- for example, types in Haskell cannot represent 20-bit immediate values for AUIPC etc

    -- However, restrictions are enforced using the type system when possible
where

import Data.Word (Word32)
import Data.Bits (zeroBits)
import qualified Data.IntMap.Strict as IM

-- | Type synonyms to increase clarity in function definitions
type Register = Word32      -- ^
type RegFile = IM.IntMap Register
type RegIdx = Int           -- ^ 
type Imm = Word32
type Prog = [Instr]
-- Type synonyms below might be oversimplified, but sufficient for now
type Memory = [Word32]

-- | https://msyksphinz-self.github.io/riscv-isadoc/
-- For now, the following (RV32I) instruction types are not supported: fence, csr, privileged, interrupt
data Instr =    Nop                             -- ^ no operation
            
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
            
            |   Lb      RegIdx Imm RegIdx       -- ^ x[rd] = sext(mem[x[rs1] + sext(offset)][7:0])
            |   Lh      RegIdx Imm RegIdx       -- ^ x[rd] = sext(mem[x[rs1] + sext(offset)][15:0])
            |   Lw      RegIdx Imm RegIdx       -- ^ x[rd] = sext(mem[x[rs1] + sext(offset)][31:0])
            |   Lbu     RegIdx Imm RegIdx       -- ^ x[rd] = mem[x[rs1] + sext(offset)][7:0]
            |   Lhu     RegIdx Imm RegIdx       -- ^ x[rd] = mem[x[rs1] + sext(offset)][15:0]
            |   Sb      RegIdx Imm RegIdx       -- ^ mem[x[rs1] + sext(offset)] = x[rs2][7:0]
            |   Sh      RegIdx Imm RegIdx       -- ^ mem[x[rs1] + sext(offset)] = x[rs2][15:0]
            |   Sw      RegIdx Imm RegIdx       -- ^ mem[x[rs1] + sext(offset)] = x[rs2][31:0]

            -- TODO: jump/branch instructions

            deriving Show
            -- insert more instructions here

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