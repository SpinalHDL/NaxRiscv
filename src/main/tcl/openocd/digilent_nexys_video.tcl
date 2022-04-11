adapter driver ftdi
ftdi_device_desc "Digilent USB Device"
ftdi_vid_pid 0x0403 0x6010
ftdi_channel 1
ftdi_layout_init 0x00e8 0x60eb
ftdi_tdo_sample_edge falling

reset_config none
adapter speed 5000

source [find cpld/xilinx-xc7.cfg]
source [find cpld/jtagspi.cfg]

set TAP_NAME xc7.tap
