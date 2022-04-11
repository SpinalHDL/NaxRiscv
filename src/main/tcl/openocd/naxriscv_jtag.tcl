set _CHIPNAME riscv
set _TARGETNAME $_CHIPNAME.cpu

jtag newtap $_CHIPNAME cpu -irlen 5 -expected-id 0x10002FFF
target create $_TARGETNAME.0 riscv -chain-position $_TARGETNAME

init
halt

echo "Ready for Remote Connections"

