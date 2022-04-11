set _CHIPNAME riscv
set _TARGETNAME $_CHIPNAME.cpu

target create $_TARGETNAME.0 riscv -chain-position $TAP_NAME
riscv use_bscan_tunnel 6 1
riscv set_bscan_tunnel_ir 0x23

init
halt

echo "Ready for Remote Connections"

