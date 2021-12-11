
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
#include <map>
#include <filesystem>
#include <chrono>

using namespace std;

#include "type.h"
#include "memory.h"
#include "elf.h"
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

class successException : public std::exception { };
#define failure() throw std::exception();
#define success() throw successException();

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

    virtual int peripheralWrite(u64 address, uint32_t length, uint8_t *data){
        switch(address){
        case 0x10000000: printf("%c", *data); break;
        default: return 1; break;
        }
        return 0;
    }

    virtual int peripheralRead(u64 address, uint32_t length, uint8_t *data){
        switch(address){
        default: return 1; break;
        }
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
		if(pendingCount != 0 && (!stall || VL_RANDOM_I(7) < 100)){
			nax->FetchCachePlugin_mem_rsp_payload_error = soc->memoryRead(address, FETCH_MEM_DATA_BYTES, (u8*)&nax->FetchCachePlugin_mem_rsp_payload_data);
			pendingCount-=FETCH_MEM_DATA_BYTES;
			address = address + FETCH_MEM_DATA_BYTES;
			nax->FetchCachePlugin_mem_rsp_valid = 1;
		}
		if(stall) nax->FetchCachePlugin_mem_cmd_ready = VL_RANDOM_I(7) < 100 && pendingCount == 0;
	}
};


#define DATA_MEM_DATA_BYTES (DATA_MEM_DATA_BITS/8)
class DataCachedReadChannel{
public:
    u64 beats;
    u64 address;
    int id;
};

class DataCachedWriteChannel{
public:
    u64 bytes;
    u64 address;
    int id;
    char buffer[DATA_LINE_BYTES];
};

class DataCachedWriteRspChannel{
public:
    bool valid;
    u64 bytes;
    u64 address;
    char data[DATA_LINE_BYTES];
};


class DataCached : public SimElement{
public:
    vector<DataCachedReadChannel> readChannels;
    DataCachedWriteChannel writeCmdChannel;
    vector<DataCachedWriteRspChannel> writeRspChannels;

    bool stall;

    VNaxRiscv* nax;
    Soc *soc;
    DataCachedReadChannel *chLock = NULL;

    DataCached(VNaxRiscv* nax, Soc *soc, bool stall){
        this->nax = nax;
        this->soc = soc;
        this->stall = stall;
        for(int i = 0;i < DATA_CACHE_REFILL_COUNT;i++) {
            DataCachedReadChannel ch;
            ch.beats = 0;
            ch.id = i;
            readChannels.push_back(ch);
        }
        writeCmdChannel.bytes = 0;

        for(int i = 0;i < DATA_CACHE_WRITEBACK_COUNT;i++) {
            DataCachedWriteRspChannel ch;
            ch.valid = false;
            writeRspChannels.push_back(ch);
        }
    }

    virtual void onReset(){
        nax->DataCachePlugin_mem_read_cmd_ready = 1;
        nax->DataCachePlugin_mem_read_rsp_valid = 0;
        nax->DataCachePlugin_mem_write_cmd_ready = 1;
        nax->DataCachePlugin_mem_write_rsp_valid = 0;
    }

    virtual void preCycle(){
        if (nax->DataCachePlugin_mem_read_cmd_valid && nax->DataCachePlugin_mem_read_cmd_ready) {
#if DATA_CACHE_REFILL_COUNT == 1
            int id = 0;
#else
            int id = nax->DataCachePlugin_mem_read_cmd_payload_id;
#endif
            assertEq("CHANNEL BUSY", readChannels[id].beats, 0);
            readChannels[id].beats = DATA_LINE_BYTES/DATA_MEM_DATA_BYTES;
            readChannels[id].address = nax->DataCachePlugin_mem_read_cmd_payload_address;
        }

        if (nax->DataCachePlugin_mem_write_cmd_valid && nax->DataCachePlugin_mem_write_cmd_ready) {
#if DATA_CACHE_WRITEBACK_COUNT == 1
            int id = 0;
#else
            int id = nax->DataCachePlugin_mem_write_cmd_payload_id;
#endif
            if(!writeCmdChannel.bytes){
                writeCmdChannel.id = id;
                writeCmdChannel.address = nax->DataCachePlugin_mem_write_cmd_payload_address;
            }
            assert(id == writeCmdChannel.id);
            assert(writeCmdChannel.address == nax->DataCachePlugin_mem_write_cmd_payload_address);
            assert(nax->DataCachePlugin_mem_write_cmd_payload_mask == (1 << DATA_MEM_DATA_BYTES)-1);

            memcpy(writeCmdChannel.buffer + writeCmdChannel.bytes, &nax->DataCachePlugin_mem_write_cmd_payload_data, DATA_MEM_DATA_BYTES);

            writeCmdChannel.bytes += DATA_MEM_DATA_BYTES;
            if(writeCmdChannel.bytes == DATA_LINE_BYTES){
                writeRspChannels[id].address = writeCmdChannel.address;
                writeRspChannels[id].bytes = writeCmdChannel.bytes;
                writeRspChannels[id].valid = true;
                memcpy(writeRspChannels[id].data, writeCmdChannel.buffer, writeCmdChannel.bytes);
                writeCmdChannel.bytes = 0;
            }
        }

    }

    virtual void postCycle(){
        // Generate read responses
        nax->DataCachePlugin_mem_read_rsp_valid = 0;
        if(!stall || VL_RANDOM_I(7) < 100){
            if(chLock == NULL){
                int id = VL_RANDOM_I(7) % DATA_CACHE_REFILL_COUNT;
                for(int i = 0;i < DATA_CACHE_REFILL_COUNT; i++){
                    if(readChannels[id].beats != 0){
                        chLock = &readChannels[id];
                        break;
                    }
                    id = (id + 1) % DATA_CACHE_REFILL_COUNT;
                }
            }

            if(chLock != NULL){
                nax->DataCachePlugin_mem_read_rsp_payload_error = soc->memoryRead(chLock->address, DATA_MEM_DATA_BYTES, (u8*)&nax->DataCachePlugin_mem_read_rsp_payload_data);
                nax->DataCachePlugin_mem_read_rsp_valid = 1;
#if DATA_CACHE_REFILL_COUNT != 0
                nax->DataCachePlugin_mem_read_rsp_payload_id = chLock->id;
#endif
                chLock->address = chLock->address + DATA_MEM_DATA_BYTES;
                chLock->beats -= 1;
                if(chLock->beats == 0){
                    chLock = NULL;
                }
            }
        }
        if(stall) nax->DataCachePlugin_mem_read_cmd_ready = VL_RANDOM_I(7) < 100;

        // Generate write responses
        nax->DataCachePlugin_mem_write_rsp_valid = 0;
        if(!stall || VL_RANDOM_I(7) < 100){
            DataCachedWriteRspChannel *ch = NULL;
            int id = VL_RANDOM_I(7) % DATA_CACHE_WRITEBACK_COUNT;
            for(int i = 0;i < DATA_CACHE_WRITEBACK_COUNT; i++){
                if(writeRspChannels[id].valid != 0){
                    ch = &writeRspChannels[id];
                    break;
                }
                id = (id + 1) % DATA_CACHE_REFILL_COUNT;
            }

            if(ch){
                ch->valid = false;
                nax->DataCachePlugin_mem_write_rsp_payload_error = soc->memoryWrite(ch->address, DATA_LINE_BYTES, (u8*)ch->data);
                nax->DataCachePlugin_mem_write_rsp_valid = 1;
#if DATA_CACHE_WRITEBACK_COUNT > 1
                nax->DataCachePlugin_mem_write_rsp_payload_id = id;
#endif
            }
        }
        if(stall) nax->DataCachePlugin_mem_write_cmd_ready = VL_RANDOM_I(7) < 100;
    }
};
//TODO randomize buses when not valid ^






class LsuPeripheral: public SimElement{
public:
    bool valid;
    bool write;
    u64 address;
    u64 data;
    u64 mask;
    u64 bytes;

    bool stall;

    VNaxRiscv* nax;
    Soc *soc;
    DataCachedReadChannel *chLock = NULL;

    LsuPeripheral(VNaxRiscv* nax, Soc *soc, bool stall){
        this->nax = nax;
        this->soc = soc;
        this->stall = stall;
        valid = false;
    }

    virtual void onReset(){
        nax->LsuPlugin_peripheralBus_cmd_ready = 0;
        nax->LsuPlugin_peripheralBus_rsp_valid = 0;
    }

    virtual void preCycle(){
        if (nax->LsuPlugin_peripheralBus_cmd_valid && nax->LsuPlugin_peripheralBus_cmd_ready) {
            assert(!valid);
            address = nax->LsuPlugin_peripheralBus_cmd_payload_address;
            data = nax->LsuPlugin_peripheralBus_cmd_payload_data;
            mask = nax->LsuPlugin_peripheralBus_cmd_payload_mask;
            bytes = 1 << nax->LsuPlugin_peripheralBus_cmd_payload_size;
            write = nax->LsuPlugin_peripheralBus_cmd_payload_write;
            valid = true;
        }
    }

    virtual void postCycle(){
        // Generate read responses
        nax->LsuPlugin_peripheralBus_rsp_valid = 0;
        if(valid && (!stall || VL_RANDOM_I(7) < 100)){
            nax->LsuPlugin_peripheralBus_rsp_valid = 1;
            u64 offset = address & (LSU_PERIPHERAL_WIDTH/8-1);
            if(write){
                nax->DataCachePlugin_mem_read_rsp_payload_error = soc->peripheralWrite(address, bytes, ((u8*) &data) + offset);
            } else {
                nax->DataCachePlugin_mem_read_rsp_payload_error = soc->peripheralRead(address, bytes, ((u8*) &nax->LsuPlugin_peripheralBus_rsp_payload_data) + offset);
            }
            valid = false;
        }
        if(stall) nax->LsuPlugin_peripheralBus_cmd_ready = VL_RANDOM_I(7) < 100;
    }
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

#ifdef ALLOCATOR_CHECKS
#include "VNaxRiscv_AllocatorMultiPortMem.h"

class NaxAllocatorChecker : public SimElement{
public:
    VNaxRiscv_AllocatorMultiPortMem* dut;
    bool busy[INTEGER_PHYSICAL_DEPTH];
    CData *push_valid[COMMIT_COUNT];
    CData *push_payload[COMMIT_COUNT];
    CData *pop_values[DISPATCH_COUNT];

    NaxAllocatorChecker(VNaxRiscv_NaxRiscv* nax): dut(nax->RfAllocationPlugin_logic_allocator),
        push_valid{MAP_INIT(&dut->io_push_,  COMMIT_COUNT, _valid)},
        push_payload{MAP_INIT(&dut->io_push_,  COMMIT_COUNT, _payload)},
        pop_values{MAP_INIT(&dut->io_pop_values_,  DISPATCH_COUNT, )} {
    }


    virtual void onReset(){
        for(int i = 0;i < INTEGER_PHYSICAL_DEPTH;i++){
            busy[i] = true;
        }
    }

    virtual void preCycle(){
        for(int i = 0;i < COMMIT_COUNT;i+=1){
            if(*push_valid[i]){
                int phys = *push_payload[i];
                assertTrue("Double free", busy[phys]);
                busy[phys] = false;
            }
        }
        if(dut->io_pop_fire){
            for(int i = 0;i < DISPATCH_COUNT;i+=1){
                if(CHECK_BIT(dut->io_pop_mask,i)){
                    int phys = *pop_values[i];
                    assertTrue("Double alloc", !busy[phys]);
                    busy[phys] = true;
                }
            }
        }
    }

    virtual void postCycle(){

    }
};

#endif

//http://www.mario-konrad.ch/blog/programming/getopt.html
enum ARG
{
    ARG_LOAD_HEX = 1,
    ARG_LOAD_ELF,
    ARG_START_SYMBOL,
    ARG_PASS_SYMBOL,
    ARG_FAIL_SYMBOL,
    ARG_OUTPUT_DIR,
    ARG_NAME,
    ARG_TIMEOUT,
    ARG_PROGRESS,
    ARG_SEED,
    ARG_TRACE,
    ARG_TRACE_REF
};


static const struct option long_options[] =
{
    { "load_hex", required_argument, 0, ARG_LOAD_HEX },
    { "load_elf", required_argument, 0, ARG_LOAD_ELF },
    { "start_symbol", required_argument, 0, ARG_START_SYMBOL },
    { "pass_symbol", required_argument, 0, ARG_PASS_SYMBOL },
    { "fail_symbol", required_argument, 0, ARG_FAIL_SYMBOL },
    { "output_dir", required_argument, 0, ARG_OUTPUT_DIR },
    { "name", required_argument, 0, ARG_NAME },
    { "timeout", required_argument, 0, ARG_TIMEOUT },
    { "progress", required_argument, 0, ARG_PROGRESS },
    { "seed", required_argument, 0, ARG_SEED },
    { "trace", no_argument, 0, ARG_TRACE },
    { "trace_ref", no_argument, 0, ARG_TRACE_REF },
    0
};

u64 startPc = 0x80000000l;
map<RvData, vector<function<void(RvData)>>> pcToEvent;
void addPcEvent(RvData pc, function<void(RvData)> func){
    if(pcToEvent.count(pc) == 0) pcToEvent[pc] = vector<function<void(RvData)>>();
    pcToEvent[pc].push_back(func);
}

int main(int argc, char** argv, char** env){
    bool trace = false;
    bool trace_ref = false;
    vluint64_t timeout = -1;
    string name = "???";
    string outputDir = "output";
    double progressPeriod = 0.0;

    while (1) {
        int index = -1;
        struct option * opt = 0;
        int result = getopt_long(argc, argv,"abc:d", long_options, &index);
        if (result == -1) break;
        switch (result) {
            case ARG_SEED: {
                Verilated::randSeed(stoi(optarg));
                srand48(stoi(optarg));
            } break;

            case ARG_TRACE: {
                trace = true;
#ifndef TRACE
                printf("You need to recompile with TRACE=yes to enable tracing");
                failure();
#endif
            } break;
            case ARG_TRACE_REF: trace_ref = true; break;
            case ARG_NAME: name = optarg; break;
            case ARG_OUTPUT_DIR: outputDir = optarg; break;
            case ARG_TIMEOUT: timeout = stoi(optarg); break;
            case ARG_PROGRESS: progressPeriod = stod(optarg); break;
            default:  break;
        }
    }

    Verilated::debug(0);
    Verilated::randReset(2);
    Verilated::traceEverOn(true);
    Verilated::commandArgs(argc, argv);
    Verilated::mkdir("logs");

    VNaxRiscv* top = new VNaxRiscv;  // Or use a const unique_ptr, or the VL_UNIQUE_PTR wrapper

    NaxWhitebox whitebox(top->NaxRiscv);

    Soc *soc;
    soc = new Soc();
	vector<SimElement*> simElements;
    simElements.push_back(new FetchCached(top, soc, true));
    simElements.push_back(new DataCached(top, soc, true));
    simElements.push_back(new LsuPeripheral(top, soc, true));
    simElements.push_back(&whitebox);
#ifdef ALLOCATOR_CHECKS
    simElements.push_back(new NaxAllocatorChecker(top->NaxRiscv));
#endif


    FILE *fptr;
    fptr = trace_ref ? fopen((outputDir + "/spike.log").c_str(),"w") : NULL;
    std::ofstream outfile ("/dev/null",std::ofstream::binary);
    sim_wrap wrap;

    processor_t proc("RV32IM", "MSU", "", &wrap, 0, false, fptr, outfile);
    if(trace_ref) proc.enable_log_commits();



	u32 nop32 = 0x13;
	u8 *nop = (u8 *)&nop32;
    Elf *elf = NULL;
    optind = 1;
    while (1) {
        int index = -1;
        struct option * opt = 0;
        int result = getopt_long(argc, argv,"abc:d", long_options, &index);
        if (result == -1) break;
        switch (result) {
            case ARG_LOAD_HEX: wrap.memory.loadHex(string(optarg)); soc->memory.loadHex(string(optarg)); break;
            case ARG_LOAD_ELF: {
                elf = new Elf(optarg);
                elf->visitBytes([&](u8 data, u64 address) {
                    wrap.memory.write(address, 1, &data);
                    soc->memory.write(address, 1, &data);
                });
            }break;
            case ARG_START_SYMBOL: startPc = elf->getSymbolAddress(optarg); break;
            case ARG_PASS_SYMBOL: {
                u64 addr = elf->getSymbolAddress(optarg);
                addPcEvent(addr, [&](RvData pc){ success();});
                wrap.memory.write(addr, 4, nop);
                soc->memory.write(addr, 4, nop);
            }break;
            case ARG_FAIL_SYMBOL:  {
                u64 addr = elf->getSymbolAddress(optarg);
                addPcEvent(addr, [&](RvData pc){ failure();});
                wrap.memory.write(addr, 4, nop);
                soc->memory.write(addr, 4, nop);
            }break;
            default:  break;
        }
    }
    /* print all other parameters */
    while (optind < argc)
    {
        printf("other parameter: <%s>\n", argv[optind++]);
    }


//    std::filesystem::remove_all(output_dir); That's toooooo much power
    std::filesystem::create_directories(outputDir);


	#ifdef TRACE
	VerilatedFstC* tfp;
	if(trace){
	    tfp = new VerilatedFstC;
	    top->trace(tfp, 99);
	    tfp->open((outputDir + "/wave.fst").c_str());
	}
    #endif


    state_t *state = proc.get_state();
    state->pc = startPc;


    auto internal = top->NaxRiscv;




//    for(int i = 0;i < 30;i++){
//        if(i == 20){
//            state->mip->write_with_mask(1 << 11, 1 << 11);
//        }
//        proc.step(1);
//        printf("%d PC= %lx\n", i,  state->pc);
//    }



    u64 commits = 0;
    int robIdChecked = 0;
    int cycleSinceLastCommit = 0;
    auto progressLast = std::chrono::high_resolution_clock::now();
    vluint64_t progressMainTimeLast = 0;

    try {
        top->clk = 0;

        for(SimElement* simElement : simElements) simElement->onReset();
        while (!Verilated::gotFinish()) {
            ++main_time;
            if(main_time == timeout){
                printf("simulation timeout\n");
                failure();
            }
            #ifdef TRACE
            if(trace){
                tfp->dump(main_time);
                if(main_time % 100000 == 0) tfp->flush();
            }
    //        		if(i == TRACE_START && i != 0) cout << "**" << endl << "**" << endl << "**" << endl << "**" << endl << "**" << endl << "START TRACE" << endl;
    //        		if(i >= TRACE_START) tfp->dump(i);
    //        		#ifdef TRACE_SPORADIC
    //        		else if(i % 1000000 < 100) tfp->dump(i);
    //        		#endif
            #endif
            if(progressPeriod != 0.0 && main_time % 20000 == 0){
                auto now = std::chrono::high_resolution_clock::now();
                auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(now-progressLast).count();
                if(elapsed > progressPeriod*1000){
                    auto cycles = (main_time-progressMainTimeLast)/2;
                    auto hz = (u64)(cycles/(elapsed/1000));
                    progressMainTimeLast = main_time;
                    progressLast = now;
                    printf("[PROGRESS] %ld cycles %ld KHz\n", cycles, hz/1000);
                }
            }
            top->clk = !top->clk;
            top->reset = (main_time < 10) ? 1 : 0;
            if(main_time < 11 && startPc != 0x80000000) top->NaxRiscv->PcPlugin_logic_fetchPc_pcReg = startPc;
            if(!top->clk || top->reset){
                top->eval();
            } else {
                for(SimElement* simElement : simElements) simElement->preCycle();

                if(cycleSinceLastCommit == 2000){
                    printf("NO PROGRESS the cpu hasn't commited anything since too long\n");
                    failure();

                }
                cycleSinceLastCommit += 1;
                for(int i = 0;i < COMMIT_COUNT;i++){
                    if(CHECK_BIT(internal->commit_mask, i)){
                        cycleSinceLastCommit = 0;
                        int robId = internal->commit_robId + i;
                        robIdChecked = robId;
                        commits += 1;
//                        printf("Commit %d %x\n", robId, whitebox.robCtx[robId].pc);
                        proc.step(1);
                        RvData pc = state->last_inst_pc;
                        assertEq("MISSMATCH PC", whitebox.robCtx[robId].pc,  pc);
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

                        if(pcToEvent.count(pc) != 0){
                            for(auto event : pcToEvent[pc]){
                                event(pc);
                            }
                        }
                        whitebox.robCtx[robId].clear();
                    }
                }
                top->eval();
                for(SimElement* simElement : simElements) simElement->postCycle();
            }
        }
    }catch (const successException e) {
        printf("SUCCESS %s\n", name.c_str());
        auto f = fopen((outputDir + "/PASS").c_str(),"w");
        fclose(f);
    } catch (const std::exception& e) {
        ++main_time;
        #ifdef TRACE
        if(trace){
        tfp->dump(main_time);
        if(main_time % 100000 == 0) tfp->flush();
        }
        #endif
        printf("LAST PC=%lx\n", state->last_inst_pc);
        printf("INCOMING PC=%lx\n", state->pc);
        printf("ROB_ID=x%x\n", robIdChecked);
        printf("FAILURE %s\n", name.c_str());
    }

//    printf("Commits=%ld\n", commits);
//    printf("Time=%ld\n", main_time);
//    printf("Cycles=%ld\n", main_time/2);
    #ifdef TRACE
    if(trace){
        tfp->flush();
        tfp->close();
    }
    #endif
    top->final();

    delete top;
    top = NULL;
    exit(0);
    return 0;
}
