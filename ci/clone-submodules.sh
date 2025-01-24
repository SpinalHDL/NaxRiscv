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

#Function to apply a patch
apply_patch() {
    local patch_dir=$1
    local patch_file=$2
    local patch_name=$3

    echo "Applying patch for $patch_name..."
    
    # Appliquer le patch et vérifier si l'application a réussi
    cd "$patch_dir" && git apply --reject "$patch_file"
    
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
            cd "$ROOT_DIR/ext/NaxSoftware"
            ./init.sh
            echo "All clones are successfully completed."
        else
            echo "Error: 'ROOT_DIR/ext/NaxSoftware/init.sh' not found!"
            echo "Please check if the submodule is correctly initialized."
            exit 1
        fi
        # Apply the patches
        apply_patch "$ROOT_DIR/ext/rvls" "$ROOT_DIR/rvls.diff" "rvls"
    else
        echo "Submodules already initialized..."
        echo " "
        echo "If you want to force reinstallation:"
        echo "  1- export NAXRISCV=\${PWD}"
        echo "  2- run 'make clean-core' from \$NAXRISCV folder"
        echo "  3- restart the script with '$0 \$NAXRISCV'"
    fi
}

# Check and clone submodules
clone_submodule
