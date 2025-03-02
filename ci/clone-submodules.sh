#!/bin/bash

# Checking the number of arguments
if [ "$#" -ne 1 ]; then
    echo "Error: You must provide exactly one argument, which is the base directory path."
    echo "Tip: Set the base directory by running the following command:"
    echo "      export NAXRISCV=\$(pwd)"
    echo "Then, run the script from the NaxRiscv directory with:"
    echo "      $0 \$NAXRISCV"
    exit 1
fi

# Arguments
ROOT_DIR=$1
RVLS_DIR="$ROOT_DIR/ext/rvls"
#RVLS_VERSION="b5c15e129f8168c2317b2c129aef91adf935bfeb"

# Function to apply a patch
apply_patch() {
    local patch_dir=$1
    local patch_file=$2
    local patch_name=$3

    echo "Applying patch for $patch_name..."
    
    # Apply the patch and verify the application succeeded
    cd "$patch_dir" || { echo "Error: Directory $patch_dir does not exist."; exit 1; }
    git apply --reject "$patch_file"
    
    if [ $? -ne 0 ]; then
        echo "Error: Patch $patch_name failed to apply. Exiting."
        exit 1
    fi

    echo "$patch_name successfully applied...OK"
    echo " "
}

# Function to check and clone submodules
clone_submodule() {
    echo "Checking and cloning submodules if necessary..."
    echo " "

    # Verifying submodules
    if git submodule status | grep -q '^-' ; then
        echo "Initializing submodules..."
        echo " "
        git submodule update --init --recursive

        # Checking for the existence of the init.sh file
        if [ -f "$ROOT_DIR/ext/NaxSoftware/init.sh" ]; then
            cd "$ROOT_DIR/ext/NaxSoftware" || exit 1
            ./init.sh
            echo "All clones are successfully completed."
        else
            echo "Error: '$ROOT_DIR/ext/NaxSoftware/init.sh' not found!"
            echo "Please check if the submodule is correctly initialized."
            exit 1
        fi
    else
        echo "Submodules already initialized..."
        echo " "
        echo "If you want to force reinstallation:"
        echo "  1- export NAXRISCV=\${PWD}"
        echo "  2- run 'make clean-core' from \$NAXRISCV folder"
        echo "  3- restart the script with '$0 \$NAXRISCV'"
    fi

    ## Ensure RVLS is at the correct version
    #echo "Ensuring RVLS is at the correct version: $RVLS_VERSION"
    #cd "$RVLS_DIR" || { echo "Error: Directory $RVLS_DIR does not exist."; exit 1; }
    #git fetch
    #git checkout "$RVLS_VERSION" || { echo "Error: Failed to checkout RVLS version $RVLS_VERSION."; exit 1; }
    #echo "RVLS successfully checked out to $RVLS_VERSION."

    # Apply patches
    apply_patch "$RVLS_DIR" "$ROOT_DIR/rvls.diff" "rvls-include-elfio"
    apply_patch "$RVLS_DIR" "$ROOT_DIR/rvls.patch" "rvls-all-modif"
    apply_patch "$ROOT_DIR/ext/SpinalHDL/lib/src/main/scala/spinal/lib/misc/test" "$ROOT_DIR/Patch_DualSimTracer_toSupportSeveralELF_addRunningLinuxFlag.patch" "DualSim"
    apply_patch "$ROOT_DIR/ext/SpinalHDL/core/src/main/scala/spinal/core/sim" "$ROOT_DIR/adding_wavePath_simConfig.patch" "simBootstraps"
}

# Main script execution
clone_submodule
