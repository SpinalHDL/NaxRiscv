# NaxRiscv

An out of order RISC-V core currently characterised by : 

- It is work in progress (currently working RV32IM)
- Target FPGA with distributed ram
- Target a (relatively) low area usage and high fmax (not the best IPC)
- Decentralized hardware elaboration (No toplevel, composed of plugins)
- Frontend implemented around a pipelining framework to ease customisation

# Running simulation

See src/test/cpp/naxriscv/README.md