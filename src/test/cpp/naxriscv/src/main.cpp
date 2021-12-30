
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
#include "disasm.h"

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

#define assertEq(message, x,ref) if((RvData)(x) != (RvData)ref) {\
	printf("\n*** %s DUT=%x REF=%x ***\n\n",message,(RvData)x,(RvData)ref);\
	breakMe();\
	failure();\
}

#define assertTrue(message, x) if(!(x)) {\
    printf("\n*** %s ***\n\n",message);\
    breakMe();\
    failure();\
}


#define BASE 0x10000000
#define PUTC BASE
#define CLINT_TIME BASE + 0x1BFF8

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
        case PUTC: printf("%c", *data); break;
        default: return 1; break;
        }
        return 0;
    }

    virtual int peripheralRead(u64 address, uint32_t length, uint8_t *data){
        switch(address){
        case CLINT_TIME:{
            u64 time = main_time/2;
            memcpy(data, &time, length);
        } break;
        case CLINT_TIME+4:{
            u64 time = (main_time/2) >> 32;
            memcpy(data, &time, length);
        } break;
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




class IoAccess{
public:
    u64 addr;
    u64 len;
    u8 data[8];
    bool write;
};

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
    queue<IoAccess> *mmioDut;
    DataCachedReadChannel *chLock = NULL;

    LsuPeripheral(VNaxRiscv* nax, Soc *soc, queue<IoAccess> *mmioDut, bool stall){
        this->nax = nax;
        this->soc = soc;
        this->stall = stall;
        this->mmioDut = mmioDut;
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
            u8 *ptr = ((u8*) &data) + offset;
            assertTrue("bad io length\n", offset + bytes <= LSU_PERIPHERAL_WIDTH/8);

            if(write){
                nax->DataCachePlugin_mem_read_rsp_payload_error = soc->peripheralWrite(address, bytes, ptr);
            } else {
                nax->DataCachePlugin_mem_read_rsp_payload_error = soc->peripheralRead(address, bytes, ptr);
                memcpy(((u8*) &nax->LsuPlugin_peripheralBus_rsp_payload_data) + offset, ptr, bytes);
            }

            IoAccess access;
            access.addr = address;
            access.len = bytes;
            access.write = write;
            memcpy(((u8*) &access.data), ptr, bytes);
            mmioDut->push(access);
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
    queue<IoAccess> mmioDut;

    // should return NULL for MMIO addresses
    virtual char* addr_to_mem(reg_t addr)  {
        if((addr & 0xF0000000) == 0x10000000) return NULL;
        return (char*)memory.get(addr);
    }
    // used for MMIO addresses
    virtual bool mmio_load(reg_t addr, size_t len, uint8_t* bytes)  {
//        printf("mmio_load %lx %ld\n", addr, len);
        assertTrue("missing mmio\n", !mmioDut.empty());
        auto dut = mmioDut.front();
        assertEq("mmio write\n", dut.write, false);
        assertEq("mmio address\n", dut.addr, addr);
        assertEq("mmio len\n", dut.len, len);
        memcpy(bytes, dut.data, len);
        mmioDut.pop();
        return true;
    }
    virtual bool mmio_store(reg_t addr, size_t len, const uint8_t* bytes)  {
//        printf("mmio_store %lx %ld\n", addr, len);
        assertTrue("missing mmio\n", !mmioDut.empty());
        auto dut = mmioDut.front();
        assertEq("mmio write\n", dut.write, true);
        assertEq("mmio address\n", dut.addr, addr);
        assertEq("mmio len\n", dut.len, len);
        assertTrue("mmio data\n", !memcmp(dut.data, bytes, len));
        mmioDut.pop();
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
    IData branchHistory;
    int opId;

    void clear(){
        integerWriteValid = false;
    }
};


class FetchCtx{
public:
    u64 fetchAt;
    u64 decodeAt;
};


class OpCtx{
public:
    int fetchId;
    int robId;

    u64 pc;
    u64 instruction;
    u64 renameAt;
    u64 dispatchAt;
    u64 issueAt;
    u64 completeAt;
    u64 commitAt;
    u64 storeAt;
    u64 counter;
    bool sqAllocated;
    int sqId;
};

class NaxStats{
public:
    u64 cycles = 0;
    u64 commits = 0;
    u64 reschedules = 0;
    u64 trap = 0;
    u64 branchMiss = 0;
    u64 jumpMiss = 0;
    u64 storeToLoadHazard = 0;

    map<RvData, u64> branchMissHist;
    map<RvData, u64> jumpMissHist;
    map<RvData, u64> pcHist;

    string report(string tab, bool hist){
        stringstream ss;
        string ret = "";
        ss << tab << "IPC               " << (double)commits/cycles <<  endl;
        ss << tab << "cycles            " << cycles <<  endl;
        ss << tab << "commits           " << commits <<  endl;
        ss << tab << "reschedules       " << reschedules <<  endl;
        ss << tab << "trap              " << trap <<  endl;
        ss << tab << "branch miss       " << branchMiss <<  endl;
        ss << tab << "jump miss         " << jumpMiss <<  endl;
        ss << tab << "storeToLoadHazard " << storeToLoadHazard <<  endl;
        if(hist){
            u64 branchCount = 0;
            ss << tab << "branch miss from :" << endl;
            for (auto const& [key, val] : branchMissHist){
                ss << tab << tab << hex << key <<" " << dec <<  setw(5) << val << " / " << pcHist[key] << endl;
                branchCount += pcHist[key];
            }
            ss << tab << "branch miss rate : " << (float)branchMiss/branchCount << endl;

            u64 jumpCount = 0;
            ss << tab << "jump miss from :" << endl;
            for (auto const& [key, val] : jumpMissHist){
                ss << tab << tab << hex << key <<" " << dec <<  std::setw(5) << val << " / " << pcHist[key] << endl;
                jumpCount += pcHist[key];
            }
            ss << tab << "jump miss rate : " << (float)jumpMiss/jumpCount << endl;

        }
        return ss.str();
    }
};

#define REASON_TRAP 0x01
#define REASON_BRANCH 0x10
#define REASON_JUMP 0x11
#define REASON_STORE_TO_LOAD_HAZARD 0x20


class NaxWhitebox : public SimElement{
public:

    VNaxRiscv_NaxRiscv* nax;
    RobCtx robCtx[ROB_SIZE];
    FetchCtx fetchCtx[4096];
    OpCtx opCtx[4096];
    int sqToOp[256];
    IData *robToPc[DISPATCH_COUNT];
    CData *integer_write_valid[INTEGER_WRITE_COUNT];
    CData *integer_write_robId[INTEGER_WRITE_COUNT];
    CData *rob_completions_valid[ROB_COMPLETIONS_PORTS];
    CData *rob_completions_payload[ROB_COMPLETIONS_PORTS];
    CData *issue_valid[ISSUE_PORTS];
    CData *issue_robId[ISSUE_PORTS];
    CData *sq_alloc_valid[DISPATCH_COUNT];
    CData *sq_alloc_id[DISPATCH_COUNT];
    SData *decoded_fetch_id[DISPATCH_COUNT];
    SData *allocated_fetch_id[DISPATCH_COUNT];
    IData *decoded_instruction[DISPATCH_COUNT];
    IData *decoded_pc[DISPATCH_COUNT];
    IData *integer_write_data[INTEGER_WRITE_COUNT];
    ofstream gem5;
    disassembler_t disasm;
    bool gem5Enable = false;

    u64 opCounter = 0;
    int periode = 2;

    bool statsCaptureEnable = true;
    NaxStats stats;

    NaxWhitebox(VNaxRiscv_NaxRiscv* nax): robToPc{MAP_INIT(&nax->robToPc_pc_,  DISPATCH_COUNT,)},
            integer_write_valid{MAP_INIT(&nax->integer_write_,  INTEGER_WRITE_COUNT, _valid)},
            integer_write_robId{MAP_INIT(&nax->integer_write_,  INTEGER_WRITE_COUNT, _robId)},
            integer_write_data{MAP_INIT(&nax->integer_write_,  INTEGER_WRITE_COUNT, _data)},
            rob_completions_valid{MAP_INIT(&nax->RobPlugin_logic_whitebox_completionsPorts_,  ROB_COMPLETIONS_PORTS, _valid)},
            rob_completions_payload{MAP_INIT(&nax->RobPlugin_logic_whitebox_completionsPorts_,  ROB_COMPLETIONS_PORTS, _payload_id)},
            issue_valid{MAP_INIT(&nax->DispatchPlugin_logic_whitebox_issuePorts_,  ISSUE_PORTS, _valid)},
            issue_robId{MAP_INIT(&nax->DispatchPlugin_logic_whitebox_issuePorts_,  ISSUE_PORTS, _payload_robId)},
            sq_alloc_valid{MAP_INIT(&nax->sqAlloc_,  DISPATCH_COUNT, _valid)},
            sq_alloc_id{MAP_INIT(&nax->sqAlloc_,  DISPATCH_COUNT, _id)},
            decoded_fetch_id{MAP_INIT(&nax->FrontendPlugin_decoded_FETCH_ID_,  DISPATCH_COUNT,)},
            decoded_instruction{MAP_INIT(&nax->FrontendPlugin_decoded_Frontend_INSTRUCTION_DECOMPRESSED_,  DISPATCH_COUNT,)},
            decoded_pc{MAP_INIT(&nax->FrontendPlugin_decoded_PC_,  DISPATCH_COUNT,)},
            disasm(XLEN){
        this->nax = nax;
    }


    void traceGem5(bool enable){
        gem5Enable = enable;
    }

    void trace(int opId){
        auto &op = opCtx[opId];
        auto &fetch = fetchCtx[op.fetchId];
        string assembly = disasm.disassemble(op.instruction);
        gem5 << "O3PipeView:fetch:" << fetch.fetchAt << ":0x" << hex <<  setw(8) << std::setfill('0') << op.pc << dec << ":0:" << op.counter << ":" << assembly << endl;
        gem5 << "O3PipeView:decode:"<< fetch.decodeAt << endl;
        gem5 << "O3PipeView:rename:"<< op.renameAt << endl;
        gem5 << "O3PipeView:dispatch:"<< op.dispatchAt << endl;
        gem5 << "O3PipeView:issue:"<< op.issueAt << endl;
        gem5 << "O3PipeView:complete:"<< op.completeAt << endl;
        gem5 << "O3PipeView:retire:" << op.commitAt << ":store:" << (op.sqAllocated ? op.storeAt : 0) << endl;
        assertTrue("a", fetch.fetchAt  <= fetch.decodeAt);
        assertTrue("b", fetch.decodeAt <= op.renameAt);
        assertTrue("c", op.renameAt    <= op.dispatchAt);
        assertTrue("d", op.dispatchAt    <= op.issueAt);
        assertTrue("e", op.issueAt    <= op.completeAt);
        if(op.sqAllocated){
            assertTrue("f", op.completeAt    <= op.commitAt);
        }
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


        if(nax->FetchPlugin_stages_1_isFirstCycle){
            auto fetchId = nax->FetchPlugin_stages_1_FETCH_ID;
            fetchCtx[fetchId].fetchAt = main_time-periode*2;
        }


        if(nax->fetchLastFire){
            auto fetchId = nax->fetchLastId;
            fetchCtx[fetchId].decodeAt = main_time;
        }

        for(int i = 0;i < DISPATCH_COUNT;i++){
            if(nax->FrontendPlugin_decoded_isFireing){
                auto fetchId = *decoded_fetch_id[i];
                auto opId = nax->FrontendPlugin_decoded_OP_ID + i;
                opCtx[opId].fetchId = fetchId;
                opCtx[opId].renameAt = main_time;
                opCtx[opId].instruction = *decoded_instruction[i];
                opCtx[opId].pc = *decoded_pc[i];
            }
            if(nax->FrontendPlugin_allocated_isFireing){
                auto robId = nax->FrontendPlugin_allocated_ROB_ID + i;
                auto opId = nax->FrontendPlugin_allocated_OP_ID + i;
                robCtx[robId].opId = opId;
                opCtx[opId].robId = robId;
            }
        }

        for(int i = 0;i < DISPATCH_COUNT;i++){
            if(nax->FrontendPlugin_dispatch_isFireing){
                auto robId = nax->FrontendPlugin_dispatch_ROB_ID + i;
                auto opId = robCtx[robId].opId;
                auto sqId = *sq_alloc_id[i];
                opCtx[opId].dispatchAt = main_time;
                opCtx[opId].sqAllocated = *sq_alloc_valid[i];
                opCtx[opId].sqId = sqId;
                if(*sq_alloc_valid[i]){
                    sqToOp[sqId] = opId;
                }
            }
        }


        for(int i = 0;i < ISSUE_PORTS;i++){
            if(*issue_valid[i]){
                opCtx[robCtx[*issue_robId[i]].opId].issueAt = main_time;
            }
        }

        for(int i = 0;i < ROB_COMPLETIONS_PORTS;i++){
            if(*rob_completions_valid[i]){
                opCtx[robCtx[*rob_completions_payload[i]].opId].completeAt = main_time;
            }
        }

        for(int i = 0;i < COMMIT_COUNT;i++){
            if((nax->commit_mask >> i) & 1){
                auto robId = nax->commit_robId + i;
                auto opId = robCtx[robId].opId;
                opCtx[opId].commitAt = main_time;
                opCtx[opId].counter = opCounter++;
                if(gem5Enable && !opCtx[opId].sqAllocated) trace(opId);
            }
        }
        if(nax->sqFree_valid){
            auto opId = sqToOp[nax->sqFree_payload];
            assertTrue("???", opCtx[opId].sqAllocated);
            opCtx[opId].storeAt = main_time;
            if(gem5Enable) trace(opId);
        }

//        if(nax->FrontendPlugin_allocated_isFireing){
//            auto robId = nax->FrontendPlugin_allocated_ROB_ID;
//            robCtx[robId].branchHistory = nax->FrontendPlugin_allocated_BRANCH_HISTORY_0;
//        }
//        for(int i = 0;i < COMMIT_COUNT;i++){
//            if((nax->commit_mask >> i) & 1){
//                auto robId = nax->commit_robId + i;
//                if(nax->HistoryPlugin_logic_onCommit_value != robCtx[robId].branchHistory) {
//                    printf("!! %ld %x %x\n", main_time, nax->HistoryPlugin_logic_onCommit_value, robCtx[robId].branchHistory);
//                    failure();
//                }
//            }
//        }
        if(statsCaptureEnable){
            stats.cycles += 1;
            for(int i = 0;i < COMMIT_COUNT;i++){
                if((nax->commit_mask >> i) & 1){
                    auto robId = nax->commit_robId + i;
                    RvData pc = robCtx[robId].pc;
                    stats.commits += 1;
                    stats.pcHist[pc] += 1;
//                    if(pc == 0x80001ed0) printf("PC commit at %ld\n", main_time);
                }
            }
            if(nax->reschedule_valid){
                RvData pc = robCtx[nax->reschedule_payload_robId].pc;
                stats.reschedules += 1;
                switch(nax->rescheduleReason){
                case REASON_TRAP: stats.trap += 1; break;
                case REASON_BRANCH: {
                    stats.branchMiss += 1;
                    stats.branchMissHist[pc] += 1;
                } break;
                case REASON_JUMP: {
                    stats.jumpMiss += 1;
                    stats.jumpMissHist[pc] += 1;
                } break;
                case REASON_STORE_TO_LOAD_HAZARD: stats.storeToLoadHazard += 1; break;
                }
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
    ARG_TRACE_REF,
    ARG_STATS_PRINT,
    ARG_STATS_PRINT_ALL,
    ARG_STATS_START_SYMBOL,
    ARG_STATS_STOP_SYMBOL,
    ARG_STATS_TOGGLE_SYMBOL,
    ARG_TRACE_GEM5,
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
    { "stats_print", no_argument, 0, ARG_STATS_PRINT },
    { "stats_print_all", no_argument, 0, ARG_STATS_PRINT_ALL },
    { "stats_start_symbol", required_argument, 0, ARG_STATS_START_SYMBOL },
    { "stats_stop_symbol", required_argument, 0, ARG_STATS_STOP_SYMBOL },
    { "stats_toggle_symbol", required_argument, 0, ARG_STATS_TOGGLE_SYMBOL },
    { "trace_gem5", no_argument, 0, ARG_TRACE_GEM5 },
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
    bool statsPrint = false;
    bool statsPrintHist = false;
    bool traceGem5 = false;

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
            case ARG_STATS_PRINT: statsPrint = true; break;
            case ARG_STATS_PRINT_ALL: statsPrint = true; statsPrintHist = true; break;
            case ARG_TRACE_GEM5: traceGem5 = true; break;
            case ARG_LOAD_HEX:
            case ARG_LOAD_ELF:
            case ARG_START_SYMBOL:
            case ARG_PASS_SYMBOL:
            case ARG_FAIL_SYMBOL:
            case ARG_STATS_START_SYMBOL:
            case ARG_STATS_STOP_SYMBOL:
            case ARG_STATS_TOGGLE_SYMBOL: break;
            default: failure(); break;
        }
    }

    Verilated::debug(0);
    Verilated::randReset(2);
    Verilated::traceEverOn(true);
    Verilated::commandArgs(argc, argv);
    Verilated::mkdir("logs");


    FILE *fptr;
    fptr = trace_ref ? fopen((outputDir + "/spike.log").c_str(),"w") : NULL;
    std::ofstream outfile ("/dev/null",std::ofstream::binary);
    sim_wrap wrap;

    processor_t proc("RV32IM", "MSU", "", &wrap, 0, false, fptr, outfile);
    if(trace_ref) proc.enable_log_commits();


    VNaxRiscv* top = new VNaxRiscv;  // Or use a const unique_ptr, or the VL_UNIQUE_PTR wrapper

    NaxWhitebox whitebox(top->NaxRiscv);
    whitebox.traceGem5(traceGem5);
    if(traceGem5) whitebox.gem5 = ofstream(outputDir + "/trace.gem5o3",std::ofstream::binary);

    Soc *soc;
    soc = new Soc();
	vector<SimElement*> simElements;
    simElements.push_back(new FetchCached(top, soc, true));
    simElements.push_back(new DataCached(top, soc, true));
    simElements.push_back(new LsuPeripheral(top, soc, &wrap.mmioDut, true));
    simElements.push_back(&whitebox);
#ifdef ALLOCATOR_CHECKS
    simElements.push_back(new NaxAllocatorChecker(top->NaxRiscv));
#endif




    u64 statsStartAt = -1;
    u64 statsStopAt = -1;
    u64 statsToggleAt = -1;

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
            case ARG_STATS_TOGGLE_SYMBOL: statsToggleAt = elf->getSymbolAddress(optarg); whitebox.statsCaptureEnable = false; break;
            case ARG_STATS_START_SYMBOL: statsStartAt = elf->getSymbolAddress(optarg); whitebox.statsCaptureEnable = false; break;
            case ARG_STATS_STOP_SYMBOL: statsStopAt = elf->getSymbolAddress(optarg); break;
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

                        if(pc == statsToggleAt) {
                            whitebox.statsCaptureEnable = !whitebox.statsCaptureEnable;
                            cout << "Stats capture " << whitebox.statsCaptureEnable << " at " << main_time << endl;
                        }
                        if(pc == statsStartAt) whitebox.statsCaptureEnable = true;
                        if(pc == statsStopAt) whitebox.statsCaptureEnable = false;
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

    if(statsPrint){
        printf("STATS :\n%s", whitebox.stats.report("  ", statsPrintHist).c_str());
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
