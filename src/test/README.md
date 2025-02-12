# NaxRiscv RISC-V Core

NaxRiscv is a high-performance RISC-V implementation with configurable extensions and verification infrastructure.

## Table of Contents

  - Quick Start

  - Directory Structure

  - Key Components

  - Installation Guide

  - Build Targets

  - Testing

  - Advanced Configuration

  - Maintenance

## Quick Start
### Prerequisites

  - Git

  - GNU Make

  - Basic build tools (gcc, g++, etc.)

```bash
# Clone repository
git clone https://github.com/SpinalHDL/NaxRiscv.git
cd NaxRiscv
export NAXRISCV=$(pwd)  # Set environment variable
git checkout rvls-update

# Full installation (toolchain + core + simulators)
make install
```

Successful installation will display:
`[SUCCESS] The entire toolchain is built successfully.`
`[SUCCESS] Verilator model for $(TARGET_NAX) generated.`

---

### Directory Structure
```bash
NaxRiscv/
├── ci/                     # CI/CD and installation scripts
├── ext/                    # Submodules
│   ├── NaxSoftware/        # Test suites (baremetal, buildroot, etc.)
│   ├── riscv-isa-sim/      # Spike simulator
│   ├── rvls/               # RVLS trace checker
│   └── SpinalHDL/          # Hardware description framework
├── simWorkspace/           # Simulation outputs
│   ├── regression/         # Regression test results
│   └── rvls/               # RVLS verification results
├── src/
│   ├── main/               # Core implementation
│   │   ├── scala/          # SpinalHDL sources
│   │   └── verilog/        # Generated RTL
│   └── test/               # Verification infrastructure
├── toolchain/              # Installed tools (Verilator, SBT, JDK)
└── tmp/                    # Temporary build files
```


### Key Components
- **Core Files**

  - `NaxRiscv.v`: Main Verilog implementation

  - `NaxRiscvSynt.v`: Synthesizable version

  - `nax.h`: Core configuration parameters

- **Configuration Patches**

  - **RVLS**: `rvls.patch`, `rvls.diff`

  - **SpinalHDL**: `adding_wavePath_simConfig.patch`, `Patch_DualSimTracer...patch`



## Installation Guide
### Step-by-Step Setup

```bash
# Initial toolchain (SBT + OpenJDK)
make install-toolchain-initial

# Full toolchain (Verilator, Spike, RVLS)
make install-toolchain

# Generate RTL for default target (rv64imafdcsu)
make NaxRiscv.v
```
Please refer to the `README.md` file located in `NaxRiscv/src/test/cpp/naxriscv/` for a detailed, step-by-step guide on setting things up independently from the main Makefile.

### Environment Variables
```bash
export JAVA_HOME=$NAXRISCV/toolchain/openjdk
export PATH=$JAVA_HOME/bin:$PATH
```


### Build Targets
|ASCII                          |Description                        |
|-------------------------------|-----------------------------------|
|`make build-spike-rvls`        |Rebuild Spike and RVLS simulators  |
|`make build-simulator`         |Rebuild regression simulator       |
|`make verilate-NaxRiscv`       |Generate Verilator model           |


## Testing

### Method 1: RVLS Core Regression Testing (SocSim + RVLS)

### 1. Single Configuration Test  
```bash
# Generate Makefile for rv64imafdcsu
cd $NAXRISCV/src/test/python/naxriscv/
./testsGenRvls.py --xlen 64 --withFloat --withDouble --withRvc --naxCount 1

# Execute tests and generate report
cd ../../../..
make test-rv64imafdcsu-all -j$(nproc)
make test-rv64imafdcsu-report
```
#### Generated Artifacts:
```bash
simWorkspace/SocDemo/
├── logs/    # Test execution logs
├── waves/   # Waveform captures
├── rtl/     # SoC Verilog source
└── verilator/ # Verilated model
```

### 2. Full Configuration Suite
```bash
# Run all predefined ISA configurations
cd $NAXRISCV
make clean-testsRvls-all
make test-rvls
```
#### Supported Configurations:
- **RV32**: `imasu`, `imacsu`, `imafcsu`, `imafdcsu`
- **RV64**: `imasu`, `imacsu`, `imafcsu`, `imafdcsu`

#### Test Automation Workflow
`NaxRiscvRvls.scala` handles:

  - Workspace setup in `simWorkspace/rvls/config_<name>`

  - Makefile generation via `testsGenRvls.py`

  - Parallel test execution

  - Result collection in `logs/` and `waves/`

---
### Method 2: Core Regression Testing

#### Run Full Test Suite:
```bash
cd $NAXRISCV
make test-regression  # Run all regression tests
```
#### NaxRiscvRegression.scala manages:

  - Workspace creation in `simWorkspace/regression/config_<name>`

  - Test setup with `testsGen.py`

  - Batch execution of all `ISA` variants

  - Result storage in `output/`

---

## Advanced Configuration

### Target Architectures
```bash
# Supported targets (32/64-bit)
TARGETS_NAX=(
  rv32imasu rv32imacsu rv32imafcsu rv32imafdcsu
  rv64imasu rv64imacsu rv64imafcsu rv64imafdcsu
)

# Example: Build RV32IMAFDCSU core
make TARGET_NAX=rv32imafdcsu NaxRiscv.v
```

### Feature Flags
|Extension                        |Parameters                           |Test Coverage          |
|---------------------------------|-------------------------------------|------------------------
|`RV32IMASU` or `RV64IMASU`       | Base ISA no parameter needed        |Base ISA               |
|`RV32IMACSU` or `RV64IMACSU`     |`--withRvc`                          |Base ISA + Compression |
|`RV32IMAFCSU` or `RV64IMAFCSU`   |`--withRvc --withFloat`              |+ Single-precision FP  |
|`RV32IMAFDCSU` or `RV64IMAFDCSU` |`--withRvc --withFloat --withDouble` |+ Double-precision FP  |


### Test Generation Parameters
#### Script Options:
```bash
cd $NAXRISCV/src/test/python/naxriscv
./testsGenRvls.py [OPTIONS]

# ISA extension parameters
--xlen=INT                  : Specify the value of xlen 32 or 64
--naxCount=INT              : Number of NaxRiscv cores
--withRvc                   : Activate Compressed Instructions Extension
--withFloat                 : Activate Single-precision floating-point extension
--withDouble                : Activate Double-precision floating-point extension
--noRva                     : Disable the Atomic extension, which is enabled by default
--no-l2                     : Disable the l2 cache, which is enabled by default

# Simulation parameters
--noRvls                    : Disable RVLS, NaxRiscv behaviour will not be checked
--dualSim                   : Enable dual lock step simulation to only trace the 50000 cycles before failure
--trace                     : Enable wave capture

# Workspace definition
--workspacePath=DIR      : Set the path in the simWorkspace folder           - default="./simWorkspace/SocDemo"
--workspaceName=DIR      : Set workspace name in workspaceDir folder         - default='.'
--workspaceOutputDir=DIR : Set Name of output directory in socSim workspace  - default='logs'
```
---

### Maintenance

#### Cleaning Commands
```bash
make clean-core         # Remove generated RTL
make clean-toolchain    # Remove toolchain installations
make clean-all          # Full cleanup (install + sim + gen)
```

#### Submodule Management
```bash
# Update submodules
git submodule update --init --recursive
```

---

### Contribution

Please ensure all scripts in `ci/` are executable:
```bash
chmod +x ci/*.sh
```