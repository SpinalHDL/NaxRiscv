
interface jtag_tcp
adapter speed 5000

set _CHIPNAME riscv
jtag newtap $_CHIPNAME cpu -irlen 5 -expected-id 0x10002FFF

set _TARGETNAME $_CHIPNAME.cpu

target create $_TARGETNAME.0 riscv -chain-position $_TARGETNAME
$_TARGETNAME.0 configure -work-area-phys 0x70000000 -work-area-size 10000 -work-area-backup 1

init
halt

#mww 0x80000000 0x01000593
#mww 0x80000004 0x01100593
#mww 0x80000008 0x01200593
#mww 0x8000000c 0x01300593
#mww 0x80000010 0x01400593
#mww 0x80000014 0x01500593
#bp 0x80000004 1 hw
#bp 0x80000010 1 hw
#reg pc 0x80000000
#echo "Doing resume"
#
#resume
#echo "Wait halt"
#wait_halt
#echo "Halted"
#echo [reg pc]
#echo [reg a1]
#rbp 0x80000004
#
#resume
#echo "Wait halt"
#wait_halt
#echo "Halted"
#echo [reg pc]
#echo [reg a1]
#rbp 0x80000010
#
#step
#echo [reg pc]
#echo [reg a1]

#reg hgeip
#reg hgeip
#reg hgeip
#exit

#load_image /media/data/open/riscv/VexRiscvOoo/ext/NaxSoftware/baremetal/debugAsm/build/rv32im/debugAsm.bin 0x80000000
#reg pc 0x80000000
#step; step; step; step; step; step; step; step;
#echo [reg pc]
#step
#echo [reg pc]


#resume
#wait_halt
#echo "step of death"
#echo [reg pc]
#echo [step]
#echo [reg pc]
#echo [step]
#echo [reg pc]
#echo "death"
#echo [targets]

#mww 0x30000000 0x11223344
#puts [mdw 0x30000000]
#mww 0x30000000 0x12345678
#mwb 0x30000004 0x11
#mwb 0x30000006 0x22
#mwb 0x30000008 0x33
#mwb 0x3000000A 0x44
#mww 0x3000000C 0xABCDEFFF
#puts [mdw 0x30000000]
#puts [mdw 0x30000000 4]

#echo "Loading image"
#load_image ../VexRiscvOoo/ext/NaxSoftware/baremetal/debug/build/rv32im/debug.bin 0x80000000
#
#echo [reg pc 0x80000000]
#
#for {set i 0} {$i < 100} {incr i} {
#  step
#  echo "step$i"
#}

#step
#echo [reg pc]
#
#step
#echo [reg pc]
#
#step
#echo [reg pc]
#
#step
#echo [reg pc]
#
#exit

echo "Ready for Remote Connections"

