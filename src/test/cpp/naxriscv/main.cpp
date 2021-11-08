
#include <verilated.h>
#include "verilated_fst_c.h"
#include "VNaxRiscv.h"
#include "VNaxRiscv_NaxRiscv.h"

#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <stdint.h>
#include <cstring>
#include <string.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <mutex>
#include <iomanip>
#include <queue>
#include <time.h>

using namespace std;

#include "memory.h"
#include "nax.h"
#define u64 uint64_t
#define u8 uint8_t

#include <stdio.h>
#include <getopt.h>



#define CALL(x,y,z) MAP_ ## x(y,z)
#define MAP_1(prefix, postfix) prefix ## 0 ## postfix
#define MAP_2(prefix, postfix) MAP_1(prefix, postfix), prefix ## 1 ## postfix
#define MAP_3(prefix, postfix) MAP_2(prefix, postfix), prefix ## 2 ## postfix
#define MAP_4(prefix, postfix) MAP_3(prefix, postfix), prefix ## 3 ## postfix
#define MAP_5(prefix, postfix) MAP_4(prefix, postfix), prefix ## 4 ## postfix
#define MAP_6(prefix, postfix) MAP_5(prefix, postfix), prefix ## 5 ## postfix
#define MAP_7(prefix, postfix) MAP_6(prefix, postfix), prefix ## 6 ## postfix
#define MAP_8(prefix, postfix) MAP_7(prefix, postfix), prefix ## 7 ## postfix
#define MAP(type, name, prefix, count, postfix) type name[] = {CALL(count, prefix, postfix)};

vluint64_t main_time = 0;


#define TEXTIFY(A) #A
void breakMe(){
    int a = 0;
}
#define assertEq(x,ref) if(x != ref) {\
	printf("\n*** %s is %d but should be %d ***\n\n",TEXTIFY(x),x,ref);\
	breakMe();\
	throw std::exception();\
}

class Soc{
public:
    Memory memory;
    virtual int memoryRead(uint32_t address,uint32_t length, uint8_t *data){
        memory.read(address, length, data);
        return 0;
    }

    virtual int memoryWrite(uint32_t address,uint32_t length, uint8_t *data){
        memory.write(address, length, data);
        return 0;
    }
};

class SimElement{
public:
	virtual ~SimElement(){}
	virtual void onReset(){}
	virtual void postReset(){}
	virtual void preCycle(){}
	virtual void postCycle(){}
};


#define FETCH_MEM_DATA_BYTES (FETCH_MEM_DATA_BITS/8)

class FetchCached : public SimElement{
public:
	bool error_next = false;
	u64 pendingCount = 0;
	u64 address;
	bool stall;

    VNaxRiscv* nax;
    Soc *soc;

	FetchCached(VNaxRiscv* nax, Soc *soc, bool stall){
		this->nax = nax;
		this->soc = soc;
		this->stall = stall;
	}

	virtual void onReset(){
		nax->FetchCachePlugin_mem_cmd_ready = 1;
		nax->FetchCachePlugin_mem_rsp_valid = 0;
	}

	virtual void preCycle(){
		if (nax->FetchCachePlugin_mem_cmd_valid && nax->FetchCachePlugin_mem_cmd_ready && pendingCount == 0) {
			assertEq(nax->FetchCachePlugin_mem_cmd_payload_address & (FETCH_MEM_DATA_BYTES-1),0);
			pendingCount = FETCH_LINE_BYTES;
			address = nax->FetchCachePlugin_mem_cmd_payload_address;
		}
	}

	virtual void postCycle(){
		nax->FetchCachePlugin_mem_rsp_valid = 0;
		if(pendingCount != 0 && (stall || VL_RANDOM_I(7) < 100)){
			nax->FetchCachePlugin_mem_rsp_payload_error = soc->memoryRead(address, FETCH_MEM_DATA_BYTES, (u8*)&nax->FetchCachePlugin_mem_rsp_payload_data);
			pendingCount-=FETCH_MEM_DATA_BYTES;
			address = address + FETCH_MEM_DATA_BYTES;
			nax->FetchCachePlugin_mem_rsp_valid = 1;
		}
		if(stall) nax->FetchCachePlugin_mem_cmd_ready = VL_RANDOM_I(7) < 100 && pendingCount == 0;
	}
};


//http://www.mario-konrad.ch/blog/programming/getopt.html
#define ARG_MEM_HEX 1
#define ARG_TIMEOUT 2
static const struct option long_options[] =
{
    { "mem_hex", required_argument, 0, ARG_MEM_HEX },
    { "timeout", required_argument, 0, ARG_TIMEOUT },
    0
};


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


    Soc *soc;

    soc = new Soc();

	vector<SimElement*> simElements;
	simElements.push_back(new FetchCached(top, soc, true));



	vluint64_t timeout = -1;

    while (1)
    {
        int index = -1;
        struct option * opt = 0;
        int result = getopt_long(argc, argv,
            "abc:d",
            long_options, &index);
        if (result == -1) break;
        switch (result)
        {
            case ARG_MEM_HEX: soc->memory.loadHexl(string(optarg)); break;
            case ARG_TIMEOUT: timeout = stoi(optarg); break;
        }
    }
    /* print all other parameters */
    while (optind < argc)
    {
        printf("other parameter: <%s>\n", argv[optind++]);
    }

    MAP(CData*, integer_write_valid, &top->NaxRiscv->integer_write_,  INTEGER_WRITE_COUNT, _valid);
    MAP(IData*, integer_write_data,  &top->NaxRiscv->integer_write_,  INTEGER_WRITE_COUNT, _data);

    try {
        top->clk = 0;


        for(SimElement* simElement : simElements) simElement->onReset();
        while (!Verilated::gotFinish()) {
            ++main_time;
            if(main_time == timeout){
                printf("simulation timeout");
                throw std::exception();
            }
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
            if(!top->clk || top->reset){
                top->eval();
            } else {
                for(SimElement* simElement : simElements) simElement->preCycle();
                for(int i = 0;i < INTEGER_WRITE_COUNT;i++){
                    if(*integer_write_valid[i]){
                        printf("RF write %d at %d\n", *integer_write_data[i], main_time);
                    }
                }
                top->eval();
                for(SimElement* simElement : simElements) simElement->postCycle();
            }
        }

    } catch (const std::exception& e) {

    }
    #ifdef TRACE
    tfp->flush();
    tfp->close();
    #endif
    top->final();

    delete top;
    top = NULL;
    exit(0);
    return 0;
}
