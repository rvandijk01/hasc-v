module HASCVTypes
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
type RegIdx = Int           -- ^ 
type Imm = Word32
type Prog = [Instr]
-- Type synonyms below might be oversimplified, but sufficient for now
type Memory = [Word32]

-- | https://msyksphinz-self.github.io/riscv-isadoc/html/rvi.html#lui
data Instr =    Nop
            |   Lui     RegIdx Imm          -- ^ x[rd] = sext(imm << 12)
            |   Auipc   RegIdx Imm          -- ^ x[rd] = pc + sext(imm << 12)
            |   Addi    RegIdx RegIdx Imm   -- ^ x[rd] = x[rs1] + sext(imm)
            |   Slti    RegIdx RegIdx Imm   -- ^ 
            |   Sltiu   RegIdx RegIdx Imm
            |   Xori    RegIdx RegIdx Imm
            |   Ori     RegIdx RegIdx Imm
            deriving Show
            -- insert more instructions here

-- | Hardware thread (hart) with local memory
data Hart = Hart { 
    regFile     :: IM.IntMap Register,
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