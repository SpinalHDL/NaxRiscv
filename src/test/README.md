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

---

### Key Components
- **Core Files**

  - `NaxRiscv.v`: Main Verilog implementation

  - `NaxRiscvSynt.v`: Synthesizable version

  - `nax.h`: Core configuration parameters

- **Configuration Patches**

  - **RVLS**: `rvls.patch`, `rvls.diff`

  - **SpinalHDL**: `adding_wavePath_simConfig.patch`, `Patch_DualSimTracer...patch`

---

### Installation Guide
#### Step-by-Step Setup

```bash
# Initial toolchain (SBT + OpenJDK)
make install-toolchain-initial

# Full toolchain (Verilator, Spike, RVLS)
make install-toolchain

# Generate RTL for default target (rv64imafdcsu)
make NaxRiscv.v
```

#### Environment Variables
```bash
export JAVA_HOME=$NAXRISCV/toolchain/openjdk
export PATH=$JAVA_HOME/bin:$PATH
```

---

### Build Targets
|ASCII                          |Description                        |
|-------------------------------|-----------------------------------|
|`make build-spike-rvls`        |Rebuild Spike and RVLS simulators  |
|`make build-simulator`         |Rebuild regression simulator       |
|`make verilate-NaxRiscv`       |Generate Verilator model           |
---------------------------------------------------------------------

---

### Testing
#### Regression Tests
```bash
make test-regression  # Run all regression tests
```

#### RVLS Verification
```bash
make test-rvls        # Validate with RVLS trace checker
```
Test results appear in `simWorkspace/regression/` and `simWorkspace/rvls/`.

---

### Advanced Configuration

#### Target Architectures
```bash
# Supported targets (32/64-bit)
TARGETS_NAX=(
  rv32imasu rv32imacsu rv32imafcsu rv32imafdcsu
  rv64imasu rv64imacsu rv64imafcsu rv64imafdcsu
)

# Example: Build RV32IMAFDCSU core
make TARGET_NAX=rv32imafdcsu NaxRiscv.v
```

#### Feature Flags
|Extension                         |Parameters                        |
|-------------------------------|-------------------------------------|
|`RV32IMACSU` or `RV64IMACSU`   |`--withRvc`                          |
|`make build-simulator`         |`--withRvc --withFloat`              |
|`make verilate-NaxRiscv`       |`--withRvc --withFloat --withDouble` |
-----------------------------------------------------------------------

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