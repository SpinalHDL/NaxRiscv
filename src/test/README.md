# NaxRiscv Build System
This document provides an overview of the Makefile for managing the NaxRiscv project. The Makefile streamlines the installation, building, and cleaning of the project and its toolchain components.

## Quick setup
To quickly set up the environment, follow these steps:
```bash
# Clone the repository
git clone https://github.com/SpinalHDL/NaxRiscv.git
cd NaxRiscv

# Set the NaxRiscv environment variable
export NAXRISCV=${PWD}

# Switch to the appropriate branch
git checkout rvls-update

# Perform the full installation
make install
```
### Description
- `git clone`: Clones the NaxRiscv repository to your local machine.
- `export NAXRISCV=${PWD}`: Sets the current directory as the environment variable NAXRISCV.
- `git checkout rvls-update`: Switches to the rvls-update branch.
- `make install`: Installs the core, toolchain, simulators, and tests.

### Output on Successful Installation
If the installation completes successfully, you will see the following messages:
- `[SUCCESS] The entire toolchain is built successfully.`
- `[SUCCESS] The Verilator model for $(TARGET_NAX) NaxRiscv has been generated.`

## Key Variables

- **`PRJ_NAX`**: The project name (default: NaxRiscv).

- **`CORE_DIR`**: Base directory of the project.

- **`TOOLCHAIN_DIR`**: Directory for storing the toolchain.

- **`INCLUDE_DIR`**: Directory for storing external includes.

- **`SPIKE_DIR`**: Path to the Spike simulator.

- **`RVLS_DIR`**: Path to RVLS simulator.

- **`RISCV_TEST_DIR`**: Directory for RISC-V tests.

## Tool Definitions

- **`RISC-V GCC`**: `$(RISCV_GCC)`
- **`Verilator`**: `$(VERILATOR_ROOT_NAX)`
- **`SBT`**: `$(TOOLCHAIN_DIR)/sbt/bin/sbt`

## Installation Targets
### Initial Toolchain Installation
```bash
make install-toolchain-initial
```
Installs initial components, including SBT and OpenJDK.


### Complete Toolchain Installation
```bash
make install-toolchain
```
Completes the toolchain installation by adding Verilator, Spike, and RVLS.


### Core Installation
```bash
make install-core
```
Clones the required submodules and applies necessary patches.

### Full Installation
```bash
make install
```
Performs a complete installation, including cloning submodules, applying patches if necessary, setting up the toolchain, installing simulators, and configuring tests.


## Build Targets
### Building Spike and RVLS
```bash
make build-spike-rvls
```
Rebuilds the Spike simulator (including ELFIO and SDL library installations) and the RVLS simulator after any modifications.


### Building Regression Simulator
```bash
make build-simulator
```
Rebuilds the regression simulator after modifications.


### Verilated Model
```bash
make verilate-NaxRiscv
```
Generates the Verilator model for NaxRiscv.


## Cleaning Targets
- **`clean-core`**: Cleans the core directory.

- **`clean-install`**: Cleans the installation files.

- **`clean-sim`**: Cleans the simulation workspace.

- **`clean-toolchain`**: Removes the toolchain directory.

- **`clean-gen`**: Cleans generated files.

### Clean All
```bash
make clean-install clean-sim clean-toolchain clean-gen
```
## Target Configurations
The Makefile supports multiple NaxRiscv targets:

- `rv32imasu`
- `rv32imacsu`
- `rv32imafcsu`
- `rv32imafdcsu`
- `rv64imasu`
- `rv64imacsu`
- `rv64imafcsu`
- `rv64imafdcsu`

### Setting a Target
Set the target by modifying the TARGET_NAX variable. For example:
```bash
make TARGET_NAX=rv64imafcsu
```

## Advanced Usage
### Parameters for Targets
Each target has predefined parameters:

- **RV32 Targets**:
  - `rv32imasu_PARAMS`: No additional parameters.
  - `rv32imacsu_PARAMS`: `--withRvc`
  - `rv32imafcsu_PARAMS`: `--withRvc --withFloat`
  - `rv32imafdcsu_PARAMS`: `--withRvc --withFloat --withDouble`

- **RV64 Targets**:
  - `rv64imasu_PARAMS`: No additional parameters.
  - `rv64imacsu_PARAMS`: `--withRvc`
  - `rv64imafcsu_PARAMS`: `--withRvc --withFloat`
  - `rv64imafdcsu_PARAMS`: `--withRvc --withFloat --withDouble`


### Generating RTL
To generate the RTL for a selected target:
```bash
make NaxRiscv.v
```
## Notes
- Export `JAVA_HOME` for Java-based tools:
    ```bash
    export JAVA_HOME=$(TOOLCHAIN_DIR)/openjdk
    ```

- Ensure all scripts in the `ci` directory are executable.
- Modify `TARGET_NAX` and other parameters as needed for your specific requirements.
