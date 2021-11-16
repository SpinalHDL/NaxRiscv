
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

#include "type.h"
#include "memory.h"
#include "nax.h"
#define u64 uint64_t
#define u8 uint8_t

#include <stdio.h>
#include <getopt.h>

#define RvAddress u32
#define RvData u32

#define CHECK_BIT(var,pos) ((var) & (1<<(pos)))

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
#define MAP_INIT(prefix, count, postfix) CALL(count, prefix, postfix)

vluint64_t main_time = 0;

#define failure() throw std::exception();

#define TEXTIFY(A) #A
void breakMe(){
    int a = 0;
}

#define assertEq(message, x,ref) if((RvData)x != (RvData)ref) {\
	printf("\n*** %s DUT=%x REF=%x ***\n\n",message,(RvData)x,(RvData)ref);\
	breakMe();\
	failure();\
}

#define assertTrue(message, x) if(!x) {\
    printf("\n*** %s ***\n\n",message);\
    breakMe();\
    failure();\
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
			assertEq("FETCH MISSALIGNED", nax->FetchCachePlugin_mem_cmd_payload_address & (FETCH_MEM_DATA_BYTES-1),0);
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


#include "processor.h"
#include "simif.h"

class sim_wrap : public simif_t{
public:

    Memory memory;
    // should return NULL for MMIO addresses
    virtual char* addr_to_mem(reg_t addr)  {
//        printf("addr_to_mem %lx\n", addr);
        return (char*)memory.get(addr);
    }
    // used for MMIO addresses
    virtual bool mmio_load(reg_t addr, size_t len, uint8_t* bytes)  {
        printf("mmio_load %lx %ld\n", addr, len);
        return true;
    }
    virtual bool mmio_store(reg_t addr, size_t len, const uint8_t* bytes)  {
        printf("mmio_store %lx %ld\n", addr, len);
        return true;
    }
    // Callback for processors to let the simulation know they were reset.
    virtual void proc_reset(unsigned id)  {
//        printf("proc_reset %d\n", id);
    }

    virtual const char* get_symbol(uint64_t addr)  {
        printf("get_symbol %lx\n", addr);
        return NULL;
    }
};

class RobCtx{
public:
    IData pc;
    bool integerWriteValid;
    RvData integerWriteData;

    void clear(){
        integerWriteValid = false;
    }
};

class NaxWhitebox : public SimElement{
public:

    VNaxRiscv_NaxRiscv* nax;
    RobCtx robCtx[ROB_SIZE];
    IData *robToPc[DISPATCH_COUNT];
    CData *integer_write_valid[INTEGER_WRITE_COUNT];
    CData *integer_write_robId[INTEGER_WRITE_COUNT];
    IData *integer_write_data[INTEGER_WRITE_COUNT];


    NaxWhitebox(VNaxRiscv_NaxRiscv* nax): robToPc{MAP_INIT(&nax->robToPc_pc_,  DISPATCH_COUNT,)},
            integer_write_valid{MAP_INIT(&nax->integer_write_,  INTEGER_WRITE_COUNT, _valid)},
            integer_write_robId{MAP_INIT(&nax->integer_write_,  INTEGER_WRITE_COUNT, _robId)},
            integer_write_data{MAP_INIT(&nax->integer_write_,  INTEGER_WRITE_COUNT, _data)} {
        this->nax = nax;
    }


    virtual void onReset(){
        for(int i = 0;i < ROB_SIZE;i++){
            robCtx[i].clear();
        }
    }

    virtual void preCycle(){
        if(nax->robToPc_valid){
            for(int i = 0;i < DISPATCH_COUNT;i++){
                robCtx[nax->robToPc_robId + i].pc = *robToPc[i];
            }
        }
        for(int i = 0;i < INTEGER_WRITE_COUNT;i++){
            if(*integer_write_valid[i]){
                auto robId = *integer_write_robId[i];
//                printf("RF write rob=%d %d at %ld\n", robId, *integer_write_data[i], main_time);
                robCtx[robId].integerWriteValid = true;
                robCtx[robId].integerWriteData = *integer_write_data[i];
            }
        }
    }

    virtual void postCycle(){

    }
};


int main(int argc, char** argv, char** env){

    printf("Miaou\n");

//    const char* isa, const char* priv, const char* varch,
//                  simif_t* sim, uint32_t id, bool halt_on_reset,
//                  FILE *log_file, std::ostream& sout_
//

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


    NaxWhitebox whitebox(top->NaxRiscv);

    Soc *soc;
    soc = new Soc();
	vector<SimElement*> simElements;
    simElements.push_back(new FetchCached(top, soc, true));
    simElements.push_back(&whitebox);




    FILE *fptr;
    fptr = fopen("spike.log","w");
    std::ofstream outfile ("spike.log2",std::ofstream::binary);
    sim_wrap wrap;

    processor_t proc("RV32IM", "MSU", "", &wrap, 0, false, fptr, outfile);
    proc.enable_log_commits();



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
            case ARG_MEM_HEX: wrap.memory.loadHex(string(optarg)); soc->memory.loadHex(string(optarg)); break;
            case ARG_TIMEOUT: timeout = stoi(optarg); break;
        }
    }
    /* print all other parameters */
    while (optind < argc)
    {
        printf("other parameter: <%s>\n", argv[optind++]);
    }



    auto internal = top->NaxRiscv;



    state_t *state = proc.get_state();
    state->pc = 0x80000000l;
//    for(int i = 0;i < 30;i++){
//        if(i == 20){
//            state->mip->write_with_mask(1 << 11, 1 << 11);
//        }
//        proc.step(1);
//        printf("%d PC= %lx\n", i,  state->pc);
//    }



    u64 commits = 0;
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


                for(int i = 0;i < COMMIT_COUNT;i++){
                    if(CHECK_BIT(internal->commit_mask, i)){
                        int robId = internal->commit_robId + i;
                        commits += 1;
//                        printf("Commit %d %x\n", robId, whitebox.robCtx[robId].pc);
                        proc.step(1);
                        assertEq("MISSMATCH PC", whitebox.robCtx[robId].pc,  state->last_inst_pc);
                        for (auto item : state->log_reg_write) {
                            if (item.first == 0)
                              continue;

                            int rd = item.first >> 4;
                            switch (item.first & 0xf) {
                            case 0: //integer
                                assertTrue("INTEGER WRITE MISSING", whitebox.robCtx[robId].integerWriteValid);
                                assertEq("INTEGER WRITE DATA", whitebox.robCtx[robId].integerWriteData, item.second.v[0]);
                              break;
                            default:
                                printf("???");
                                failure();
                                break;
                            }
                        }

                        whitebox.robCtx[robId].clear();
                    }
                }
                top->eval();
                for(SimElement* simElement : simElements) simElement->postCycle();
            }
        }

    } catch (const std::exception& e) {
        ++main_time;
        #ifdef TRACE
        tfp->dump(main_time);
        if(main_time % 100000 == 0) tfp->flush();
        #endif
        printf("REF PC=%lx\n", state->last_inst_pc);
        printf("Commits=%d\n", commits);
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
