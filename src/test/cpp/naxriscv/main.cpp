
#include <verilated.h>
#include "verilated_fst_c.h"
#include "VNaxRiscv.h"

#include <stdio.h>

vluint64_t main_time = 0;
int main(int argc, char** argv, char** env){
    printf("Miaou\n");
    // This example started with the Verilator example files.
    // Please see those examples for commented sources, here:
    // https://github.com/verilator/verilator/tree/master/examples

//    if (0 && argc && argv && env) {}

    Verilated::debug(0);
    Verilated::randReset(2);
    Verilated::traceEverOn(true);
    Verilated::commandArgs(argc, argv);
    Verilated::mkdir("logs");

    VNaxRiscv* top = new VNaxRiscv;  // Or use a const unique_ptr, or the VL_UNIQUE_PTR wrapper
	#ifdef TRACE
	VerilatedFstC* tfp;
	#endif
    #ifdef TRACE
    Verilated::traceEverOn(true);
    tfp = new VerilatedFstC;
    top->trace(tfp, 99);
    tfp->open("wave.fst");
    #endif
    top->clk = 0;
    while (!Verilated::gotFinish()) {
        ++main_time;
        #ifdef TRACE
        tfp->dump(main_time);
        if(main_time % 100000 == 0) tfp->flush();
//        		if(i == TRACE_START && i != 0) cout << "**" << endl << "**" << endl << "**" << endl << "**" << endl << "**" << endl << "START TRACE" << endl;
//        		if(i >= TRACE_START) tfp->dump(i);
//        		#ifdef TRACE_SPORADIC
//        		else if(i % 1000000 < 100) tfp->dump(i);
//        		#endif
        #endif
        top->clk = !top->clk;
        top->reset = (main_time < 10) ? 1 : 0;
        top->eval();
    }

    top->final();

    //  Coverage analysis (since test passed)


    delete top;
    top = NULL;
    exit(0);
    return 0;
}