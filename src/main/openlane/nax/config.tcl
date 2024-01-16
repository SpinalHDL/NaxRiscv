
set ::env(DESIGN_NAME) {nax}
set ::env(VERILOG_FILES) [glob $::env(DESIGN_DIR)/src/*.v]
set ::env(CLOCK_PORT) "clk"
set ::env(CLOCK_PERIOD) "50.0"
set ::env(DESIGN_IS_CORE) {1}

# If you use SRAM macro, uncomment this
#set ::env(VERILOG_FILES_BLACKBOX) [glob $::env(DESIGN_DIR)/sram/*.v]
#set ::env(EXTRA_LEFS) [glob $::env(DESIGN_DIR)/sram/*.lef]
#set ::env(EXTRA_GDS_FILES) [glob $::env(DESIGN_DIR)/sram/*.gds]
#set ::env(EXTRA_LIBS) [glob $::env(DESIGN_DIR)/sram/*_TT_1p8V_25C.lib]
#set ::env(MACRO_PLACEMENT_CFG) $::env(DESIGN_DIR)/macro_placement.cfg

set ::env(FP_SIZING) "absolute"
set ::env(DIE_AREA)  "0 0 3600 3600"

set ::env(FP_CORE_UTIL) "20"
set ::env(PL_TARGET_DENSITY) "0.25"

# Attempts to workaround https://github.com/The-OpenROAD-Project/OpenLane/issues/2030
set ::env(FP_PDN_HORIZONTAL_HALO) "40"
set ::env(FP_PDN_VERTICAL_HALO) "40"
set ::env(PL_MACRO_HALO) "2 2"

# yosys Error: The network is combinational workaround
set ::env(SYNTH_STRATEGY) {AREA 3}

set tech_specific_config "$::env(DESIGN_DIR)/$::env(PDK)_$::env(STD_CELL_LIBRARY)_config.tcl"
if { [file exists $tech_specific_config] == 1 } {
    source $tech_specific_config
}


