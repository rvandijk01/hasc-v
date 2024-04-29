# HASC-V

This project aims to simulate a 'simple' RISC-V processor in Haskell supporting parallel execution and hopefully many RISC-V extensions. It is done as a hobby/educational project.

It takes inspiration from the [Sprockell](https://github.com/martijnbastiaan/sprockell) project.

## Features
- Simulation of a single RV32I hardware thread except jump/branch instructions (coming soon).
- Work in progress (see roadmap below)

## Roadmap (decreasing order of priority)
- Support simulation of a single RV32IM hardware thread (hart) as defined in the [RISC-V unprivileged spec version 20191213](https://github.com/riscv/riscv-isa-manual/releases/tag/Ratified-IMAFDQC)
- Translation from executable/binary format to Embedded Domain Specific Language (EDSL) as defined in HASCVTypes.hs
- Support simulation of parallel execution of multiple harts along with shared memory.
- Support RISC-V's "A" standard extension for Atomic Instructions to allow synchronization between parallel harts.
- Support RISC-V's "C" standard extension for Compressed Instructions in the binary translation layer.
- Support the Machine-Level ISA as defined in the [RISC-V privileged spec version 20211203](https://github.com/riscv/riscv-isa-manual/releases/tag/Priv-v1.12)
- Suggestions are welcome