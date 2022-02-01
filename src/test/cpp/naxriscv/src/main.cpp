
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
#include <unistd.h>
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

#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <arpa/inet.h>

#include "disasm.h"

using namespace std;

#define VL_RANDOM_I_WIDTH(w) (VL_RANDOM_I() & (1 << w)-1)

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

#define SIM_MASTER_PORT 18654
#define SA struct sockaddr

class successException : public std::exception { };
#define failure() throw std::exception();
#define success() throw successException();

#define TEXTIFY(A) #A
void breakMe(){
    int a = 0;
}

#define assertEq(message, x,ref) if((RvData)(x) != (RvData)(ref)) {\
	printf("\n*** %s DUT=%x REF=%x ***\n\n",message,(RvData)x,(RvData)ref);\
	breakMe();\
	failure();\
}

#define assertTrue(message, x) if(!(x)) {\
    printf("\n*** %s ***\n\n",message);\
    breakMe();\
    failure();\
}

#include <sys/stat.h>

//Thanks https://stackoverflow.com/questions/675039/how-can-i-create-directory-tree-in-c-linux
int mkpath(std::string s,mode_t mode)
{
    size_t pos=0;
    std::string dir;
    int mdret;

    if(s[s.size()-1]!='/'){
        // force trailing / so we can handle everything in loop
        s+='/';
    }

    while((pos=s.find_first_of('/',pos))!=std::string::npos){
        dir=s.substr(0,pos++);
        if(dir.size()==0) continue; // if leading / first time is 0 length
        if((mdret=mkdir(dir.c_str(),mode)) && errno!=EEXIST){
            return mdret;
        }
    }
    return mdret;
}


#define CAUSE_MACHINE_SOFTWARE 3
#define CAUSE_MACHINE_TIMER 7
#define CAUSE_MACHINE_EXTERNAL 11
#define CAUSE_SUPERVISOR_EXTERNAL 9


#define MIE_MTIE (1 << CAUSE_MACHINE_TIMER)
#define MIE_MEIE (1 << CAUSE_MACHINE_EXTERNAL)
#define MIE_MSIE (1 << CAUSE_MACHINE_SOFTWARE)
#define MIE_SEIE (1 << CAUSE_SUPERVISOR_EXTERNAL)

#include "encoding.h"
#define MIP 0x344
#define SIP 0x144
#define UIP  0x44


#define BASE 0x10000000
#define PUTC BASE
#define PUT_HEX (BASE + 0x8)
#define CLINT_BASE (BASE + 0x10000)
#define CLINT_TIME (CLINT_BASE + 0x0BFF8)
#define MACHINE_EXTERNAL_INTERRUPT_CTRL (BASE+0x10)
#define SUPERVISOR_EXTERNAL_INTERRUPT_CTRL (BASE + 0x18)
#define GETC (BASE + 0x40)

#define MM_FAULT_ADDRESS 0x00001230
#define IO_FAULT_ADDRESS 0x1FFFFFF0


#define CLINT_CMP_ADDR (CLINT_BASE + 0x4000)

bool traceWave = false;
bool trace_ref = false;
vluint64_t timeout = -1;
string simName = "???";
string outputDir = "output";
double progressPeriod = 0.0;
bool statsPrint = false;
bool statsPrintHist = false;
bool traceGem5 = false;
bool spike_debug = false;
bool simMaster = false;
bool simSlave = false;
bool noStdIn = false;
bool putcFlush = true;


class TestSchedule{
public:
    virtual void activate() = 0;
};


queue <TestSchedule*> testScheduleQueue;

void testScheduleQueueNext(){
    if(testScheduleQueue.empty()) return;
    auto e = testScheduleQueue.front();
    testScheduleQueue.pop();
    e->activate();
}



inline bool ends_with(std::string const & value, std::string const & ending)
{
    if (ending.size() > value.size()) return false;
    return std::equal(ending.rbegin(), ending.rend(), value.rbegin());
}

void simMasterGetC(char c);

bool stdinNonEmpty(){
  struct timeval tv;
  fd_set fds;
  tv.tv_sec = 0;
  tv.tv_usec = 0;
  FD_ZERO(&fds);
  FD_SET(STDIN_FILENO, &fds);
  select(STDIN_FILENO+1, &fds, NULL, NULL, &tv);
  return (FD_ISSET(0, &fds));
}



class SimElement{
public:
	virtual ~SimElement(){}
	virtual void onReset(){}
	virtual void postReset(){}
	virtual void preCycle(){}
	virtual void postCycle(){}
};

class Soc : public SimElement{
public:
    Memory memory;
    VNaxRiscv* nax;
    u64 clintCmp = 0;
    queue <char> customCin;
    string putcHistory = "";
    string *putcTarget = NULL;

    Soc(VNaxRiscv* nax){
        this->nax = nax;
    }
    virtual int memoryRead(uint32_t address,uint32_t length, uint8_t *data){
        if(address < 0x10000000) return 1;
        memory.read(address, length, data);
        return 0;
    }

    virtual int memoryWrite(uint32_t address,uint32_t length, uint8_t *data){
        if(address < 0x10000000) return 1;
        memory.write(address, length, data);
        return 0;
    }

    virtual int peripheralWrite(u64 address, uint32_t length, uint8_t *data){
        switch(address){
        case PUTC: {
            printf("%c", *data); if(putcFlush) fflush(stdout);
            putcHistory += (char)(*data);
            if(putcTarget){
                if(ends_with(putcHistory, *putcTarget)){
                    putcTarget = NULL;
                    testScheduleQueueNext();
                    putcHistory = "";
                }
            }
        }break;
        case PUT_HEX: printf("%lx", *((u64*)data)); if(putcFlush) fflush(stdout); break;
        case MACHINE_EXTERNAL_INTERRUPT_CTRL: nax->PrivilegedPlugin_io_int_machine_external = *data & 1;  break;
        #if SUPERVISOR == 1
        case SUPERVISOR_EXTERNAL_INTERRUPT_CTRL: nax->PrivilegedPlugin_io_int_supervisor_external = *data & 1;  break;
        #endif
        case CLINT_BASE: nax->PrivilegedPlugin_io_int_machine_software = *data & 1;  break;
        case CLINT_CMP_ADDR: memcpy(&clintCmp, data, length); /*printf("CMPA=%lx\n", clintCmp);*/ break;
        case CLINT_CMP_ADDR+4: memcpy(((char*)&clintCmp)+4, data, length); /*printf("CMPB=%lx\n", clintCmp);*/  break;
        default: return 1; break;
        }
        return 0;
    }

    virtual int peripheralRead(u64 address, uint32_t length, uint8_t *data){
        switch(address){
        case GETC:{
            if(!simSlave && !noStdIn && stdinNonEmpty()){
                char c;
                auto dummy = read(0, &c, 1);
                memset(data, 0, length);
                *data = c;
                if(simMaster){
                    simMasterGetC(c);
                }
            } else if(!customCin.empty()){
                memset(data, 0, length);
                *data = customCin.front();
                customCin.pop();
            } else {
                memset(data, 0xFF, length);
            }
        } break;
        case CLINT_TIME:{
            u64 time = main_time/2;
            memcpy(data, &time, length);
        } break;
        case CLINT_TIME+4:{
            u64 time = (main_time/2) >> 32;
            memcpy(data, &time, length);
        } break;
        case CLINT_CMP_ADDR:  memcpy(data, &clintCmp, length); break;
        case CLINT_CMP_ADDR+4:memcpy(data, ((char*)&clintCmp) + 4, length); break;
        default: return 1; break;
        }
        return 0;
    }


    virtual void onReset(){
        nax->PrivilegedPlugin_io_int_machine_external = 0;
        nax->PrivilegedPlugin_io_int_machine_timer = 0;
        nax->PrivilegedPlugin_io_int_machine_software = 0;
        nax->PrivilegedPlugin_io_int_supervisor_external = 0;
    }

    virtual void preCycle(){

    }

    virtual void postCycle(){
        nax->PrivilegedPlugin_io_int_machine_timer = clintCmp < (main_time/2);
    }
};


Soc *soc;

class WaitPutc : public TestSchedule{
public:
    WaitPutc(string putc) : putc(putc){}
    string putc;
    void activate() {
        soc->putcTarget = &putc;
    }
};


class DoSuccess: public TestSchedule{
public:
    void activate() {
        success();
    }
};



class DoGetc : public TestSchedule{
public:
    string getc;
    DoGetc(string getc) : getc(getc){}
    void activate() {
        for(char e : getc){
            soc->customCin.push(e);
        }
        soc->customCin.push('\n');
        testScheduleQueueNext();
    }
};


class WithMemoryLatency{
public:
    virtual void setLatency(int cycles) = 0;
};

#define FETCH_MEM_DATA_BYTES (FETCH_MEM_DATA_BITS/8)

class FetchCached : public SimElement, public WithMemoryLatency{
public:
	bool error_next = false;
	u64 pendingCount = 0;
	u64 address;
	bool stall;

    VNaxRiscv* nax;
    Soc *soc;

    u32 readyTrigger = 100;
    void setLatency(int cycles){
        readyTrigger = 128.0*(1+FETCH_LINE_BYTES/FETCH_MEM_DATA_BYTES)/cycles;
    }

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
		if(pendingCount != 0 && (!stall || VL_RANDOM_I_WIDTH(7) < readyTrigger)){
			nax->FetchCachePlugin_mem_rsp_payload_error = soc->memoryRead(address, FETCH_MEM_DATA_BYTES, (u8*)&nax->FetchCachePlugin_mem_rsp_payload_data);
			pendingCount-=FETCH_MEM_DATA_BYTES;
			address = address + FETCH_MEM_DATA_BYTES;
			nax->FetchCachePlugin_mem_rsp_valid = 1;
		}
		if(stall) nax->FetchCachePlugin_mem_cmd_ready = VL_RANDOM_I_WIDTH(7) < readyTrigger && pendingCount == 0;
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


class DataCached : public SimElement, public WithMemoryLatency{
public:
    vector<DataCachedReadChannel> readChannels;
    DataCachedWriteChannel writeCmdChannel;
    vector<DataCachedWriteRspChannel> writeRspChannels;

    bool stall;

    VNaxRiscv* nax;
    Soc *soc;
    DataCachedReadChannel *chLock = NULL;

    u32 readyTrigger = 100;
    void setLatency(int cycles){
        readyTrigger = 128.0*(1+DATA_LINE_BYTES/DATA_MEM_DATA_BYTES)/cycles;
    }

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
        if(!stall || VL_RANDOM_I_WIDTH(7) < readyTrigger){
            if(chLock == NULL){
                int id = VL_RANDOM_I_WIDTH(7) % DATA_CACHE_REFILL_COUNT;
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
        if(stall) nax->DataCachePlugin_mem_read_cmd_ready = VL_RANDOM_I_WIDTH(7) < readyTrigger;

        // Generate write responses
        nax->DataCachePlugin_mem_write_rsp_valid = 0;
        if(!stall || VL_RANDOM_I_WIDTH(7) < readyTrigger){
            DataCachedWriteRspChannel *ch = NULL;
            int id = VL_RANDOM_I_WIDTH(7) % DATA_CACHE_WRITEBACK_COUNT;
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
        if(stall) nax->DataCachePlugin_mem_write_cmd_ready = VL_RANDOM_I_WIDTH(7) < readyTrigger;
    }
};
//TODO randomize buses when not valid ^




class IoAccess{
public:
    u64 addr;
    u64 len;
    u8 data[8];
    bool write;
    bool error;
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
        if(valid && (!stall || VL_RANDOM_I_WIDTH(7) < 100)){
            nax->LsuPlugin_peripheralBus_rsp_valid = 1;
            u64 offset = address & (LSU_PERIPHERAL_WIDTH/8-1);
            u8 *ptr = ((u8*) &data) + offset;
            assertTrue("bad io length\n", offset + bytes <= LSU_PERIPHERAL_WIDTH/8);

            if(write){
                nax->LsuPlugin_peripheralBus_rsp_payload_error = soc->peripheralWrite(address, bytes, ptr);
            } else {
                nax->LsuPlugin_peripheralBus_rsp_payload_error = soc->peripheralRead(address, bytes, ptr);
                memcpy(((u8*) &nax->LsuPlugin_peripheralBus_rsp_payload_data) + offset, ptr, bytes);
            }

            IoAccess access;
            access.addr = address;
            access.len = bytes;
            access.write = write;
            access.error = nax->LsuPlugin_peripheralBus_rsp_payload_error;
            memcpy(((u8*) &access.data), ptr, bytes);
            mmioDut->push(access);
            valid = false;
        }
        if(stall) nax->LsuPlugin_peripheralBus_cmd_ready = VL_RANDOM_I_WIDTH(7) < 100;
    }
};


#include "processor.h"
#include "mmu.h"
#include "simif.h"



class sim_wrap : public simif_t{
public:

    Memory memory;
    queue<IoAccess> mmioDut;

    // should return NULL for MMIO addresses
    virtual char* addr_to_mem(reg_t addr)  {
        if((addr & 0xE0000000) == 0x00000000) return NULL;
        return (char*)memory.get(addr);
    }
    // used for MMIO addresses
    virtual bool mmio_load(reg_t addr, size_t len, uint8_t* bytes)  {
//        printf("mmio_load %lx %ld\n", addr, len);
        if(addr < 0x10000000) return false;
        assertTrue("missing mmio\n", !mmioDut.empty());
        auto dut = mmioDut.front();
        assertEq("mmio write\n", dut.write, false);
        assertEq("mmio address\n", dut.addr, addr);
        assertEq("mmio len\n", dut.len, len);
        memcpy(bytes, dut.data, len);
        mmioDut.pop();
        return !dut.error;
    }
    virtual bool mmio_store(reg_t addr, size_t len, const uint8_t* bytes)  {
//        printf("mmio_store %lx %ld\n", addr, len);
        if(addr < 0x10000000) return false;
        assertTrue("missing mmio\n", !mmioDut.empty());
        auto dut = mmioDut.front();
        assertEq("mmio write\n", dut.write, true);
        assertEq("mmio address\n", dut.addr, addr);
        assertEq("mmio len\n", dut.len, len);
        assertTrue("mmio data\n", !memcmp(dut.data, bytes, len));
        mmioDut.pop();
        return !dut.error;
    }
    // Callback for processors to let the simulation know they were reset.
    virtual void proc_reset(unsigned id)  {
//        printf("proc_reset %d\n", id);
    }

    virtual const char* get_symbol(uint64_t addr)  {
//        printf("get_symbol %lx\n", addr);
        return NULL;
    }
};

class RobCtx{
public:
    IData pc;
    bool integerWriteValid;
    RvData integerWriteData;

    bool csrValid;
    bool csrWriteDone;
    bool csrReadDone;
    RvData csrAddress;
    RvData csrWriteData;
    RvData csrReadData;

    IData branchHistory;
    int opId;

    void clear(){
        integerWriteValid = false;
        csrValid = false;
        csrWriteDone = false;
        csrReadDone = false;
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
            for (auto const& x : branchMissHist){
                auto key = x.first;
                auto val = x.second;
                ss << tab << tab << hex << key <<" " << dec <<  setw(5) << val << " / " << pcHist[key] << endl;
                branchCount += pcHist[key];
            }
            ss << tab << "branch miss rate : " << (float)branchMiss/branchCount << endl;

            u64 jumpCount = 0;
            ss << tab << "jump miss from :" << endl;
            for (auto const& x : jumpMissHist){
                auto key = x.first;
                auto val = x.second;
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
    CData *dispatch_mask[DISPATCH_COUNT];
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
            dispatch_mask{MAP_INIT(&nax->FrontendPlugin_dispatch_Frontend_DISPATCH_MASK_,  DISPATCH_COUNT,)},
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

        if (nax->FrontendPlugin_dispatch_isFireing) {
            for (int i = 0; i < DISPATCH_COUNT; i++) {
                if (*dispatch_mask[i]) {
                    auto robId = nax->FrontendPlugin_dispatch_ROB_ID + i;
                    auto opId = robCtx[robId].opId;
                    auto sqId = *sq_alloc_id[i];
                    opCtx[opId].dispatchAt = main_time;
                    opCtx[opId].sqAllocated = *sq_alloc_valid[i];
                    opCtx[opId].sqId = sqId;
                    if (*sq_alloc_valid[i]) {
                        sqToOp[sqId] = opId;
                    }
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
            assertTrue("??? at sqFree", opCtx[opId].sqAllocated);
            opCtx[opId].storeAt = main_time;
            if(gem5Enable) trace(opId);
        }
        if(nax->csrAccess_valid){
            auto robId = nax->csrAccess_payload_robId;
//                printf("RF write rob=%d %d at %ld\n", robId, *integer_write_data[i], main_time);
            robCtx[robId].csrValid = true;
            robCtx[robId].csrAddress = nax->csrAccess_payload_address;
            robCtx[robId].csrWriteDone = nax->csrAccess_payload_writeDone;
            robCtx[robId].csrReadDone = nax->csrAccess_payload_readDone;
            robCtx[robId].csrWriteData = nax->csrAccess_payload_write;
            robCtx[robId].csrReadData = nax->csrAccess_payload_read;
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



int syncSockfd, syncConnfd;


//http://www.mario-konrad.ch/blog/programming/getopt.html
enum ARG
{
    ARG_LOAD_HEX = 1,
    ARG_LOAD_ELF,
    ARG_LOAD_BIN,
    ARG_START_SYMBOL,
    ARG_PASS_SYMBOL,
    ARG_FAIL_SYMBOL,
    ARG_OUTPUT_DIR,
    ARG_NAME,
    ARG_TIMEOUT,
    ARG_PROGRESS,
    ARG_SEED,
    ARG_TRACE,
    ARG_TRACE_START_TIME,
    ARG_TRACE_STOP_TIME,
    ARG_TRACE_SPORADIC,
    ARG_TRACE_REF,
    ARG_STATS_PRINT,
    ARG_STATS_PRINT_ALL,
    ARG_STATS_START_SYMBOL,
    ARG_STATS_STOP_SYMBOL,
    ARG_STATS_TOGGLE_SYMBOL,
    ARG_TRACE_GEM5,
    ARG_SPIKE_DEBUG,
    ARG_MEMORY_LATENCY,
    ARG_SIM_MASTER,
    ARG_SIM_SLAVE,
    ARG_SIM_SLAVE_DELAY,
    ARG_NO_STDIN,
    ARG_NO_PUTC_FLUSH,
    ARG_PUTC,
    ARG_GETC,
    ARG_SUCCESS,
    ARG_HELP,
};


static const struct option long_options[] =
{
    { "help", no_argument, 0, ARG_HELP },
    { "load-hex", required_argument, 0, ARG_LOAD_HEX },
    { "load-elf", required_argument, 0, ARG_LOAD_ELF },
    { "load-bin", required_argument, 0, ARG_LOAD_BIN },
    { "start-symbol", required_argument, 0, ARG_START_SYMBOL },
    { "pass-symbol", required_argument, 0, ARG_PASS_SYMBOL },
    { "fail-symbol", required_argument, 0, ARG_FAIL_SYMBOL },
    { "output-dir", required_argument, 0, ARG_OUTPUT_DIR },
    { "name", required_argument, 0, ARG_NAME },
    { "timeout", required_argument, 0, ARG_TIMEOUT },
    { "progress", required_argument, 0, ARG_PROGRESS },
    { "seed", required_argument, 0, ARG_SEED },
    { "trace", no_argument, 0, ARG_TRACE },
    { "trace-start-time", required_argument, 0, ARG_TRACE_START_TIME },
    { "trace-stop-time", required_argument, 0, ARG_TRACE_STOP_TIME },
    { "trace-sporadic", required_argument, 0, ARG_TRACE_SPORADIC },
    { "trace-ref", no_argument, 0, ARG_TRACE_REF },
    { "stats-print", no_argument, 0, ARG_STATS_PRINT },
    { "stats-print-all", no_argument, 0, ARG_STATS_PRINT_ALL },
    { "stats-start-symbol", required_argument, 0, ARG_STATS_START_SYMBOL },
    { "stats-stop-symbol", required_argument, 0, ARG_STATS_STOP_SYMBOL },
    { "stats-toggle-symbol", required_argument, 0, ARG_STATS_TOGGLE_SYMBOL },
    { "trace-gem5", no_argument, 0, ARG_TRACE_GEM5 },
    { "spike-debug", no_argument, 0, ARG_SPIKE_DEBUG },
    { "memory-latency", required_argument, 0, ARG_MEMORY_LATENCY },
    { "sim-master", no_argument, 0, ARG_SIM_MASTER },
    { "sim-slave", no_argument, 0, ARG_SIM_SLAVE },
    { "sim-slave-delay", required_argument, 0, ARG_SIM_SLAVE_DELAY },
    { "no-stdin", no_argument, 0, ARG_NO_STDIN },
    { "no-putc-flush", no_argument, 0, ARG_NO_PUTC_FLUSH },
    { "putc", required_argument, 0, ARG_PUTC },
    { "getc", required_argument, 0, ARG_GETC },
    { "success", no_argument, 0, ARG_SUCCESS },
    0
};


string helpString = R"(
--help                  : Print this

Simulation setup
--load-bin=FILE,ADDRESS : Load a binary file in the simulation memory at the given hexadecimal address. ex file,80000000
--load-hex=FILE         : Load a hex file in the simulation memory
--load-elf=FILE         : Load a elf file in the simulation memory
--start-symbol=SYMBOL   : Force the CPU to boot at the given elf symbol
--pass-symbol=SYMBOL    : The simulation will pass when the given elf symbol execute
--fail-symbol=SYMBOL    : The simulation will fail when the given elf symbol execute
--timeout=INT           : Simulation time before failure (~number of cycles x 2)
--seed=INT              : Seed used to initialize randomizers
--memory-latency=CYCLES : Specify the average memory latency from cmd to the last rsp beat
--no-stdin              : Do not redirect the terminal stdin to the simulated getc
--no-putc-flush         : The sim will not flush the terminal stdout after each sim putc
--name=STRING           : Test name reported when on exit (not very useful XD)

Simulation tracing / probing
--output-dir=DIR        : Path to where every traces will be written
--progress=PERIOD       : Will print the simulation speed each period seconds
--trace                 : Enable FST wave capture
--trace-start-time=INT  : Add a time to which the FST should start capturing
--trace-stop-time=INT   : Add a time to which the FST should stop capturng
--trace-sporadic=RATIO  : Specify that periodically the FST capture a bit of the wave
--trace-ref             : Store the spike execution traces in a file
--stats-print           : Print some stats about the CPU execution at the end of the sim
--stats-print-all       : Print all the stats possible (including which branches had miss)
--stats-start-symbol=SY : Specify at which elf symbol the stats should start capturing
--stats-stop-symbol=SYM : Specify at which elf symbol the stats should stop capturing
--stats-toggle-symbol=S : Specify at which elf symbol the stats should change its capture state
--trace-gem5            : Enable capture of the pipeline timings as a gem5 trace, readable with github konata
--spike-debug           : Enable spike debug mode (more verbose traces)
--sim-master            : The simulation will wait a sim-slave to connect and then run until pass/fail
--sim-slave             : The simulation will connect to a sim-master and then run behind it
                          When the sim-master fail, then the sim-slave will run to that point with trace enabled
--sim-slave-delay=TIME  : For the sim-slave, specify how much behind the sim-master it has to be.

Directed test argument : Used, for instance, to automate the shell interactions in the linux regression
--putc=STRING          : Send the given string to the sim getc
--getc=STRING          : Wait the sim to putc the given string
--success              : Quit the simulation successfully
)";

u64 startPc = 0x80000000l;
map<RvData, vector<function<void(RvData)>>> pcToEvent;
map<u64, vector<function<void()>>> timeToEvent;
void addPcEvent(RvData pc, function<void(RvData)> func){
    if(pcToEvent.count(pc) == 0) pcToEvent[pc] = vector<function<void(RvData)>>();
    pcToEvent[pc].push_back(func);
}

void addTimeEvent(u64 time, function<void()> func){
    if(timeToEvent.count(time) == 0) timeToEvent[time] = vector<function<void()>>();
    timeToEvent[time].push_back(func);
}


bool trace_enable = true;
float trace_sporadic_factor = 0.0f;
u64 trace_sporadic_period = 100000;
u64 trace_sporadic_trigger;
processor_t *proc;
sim_wrap *wrap;
state_t *state;
FILE *fptr;
VNaxRiscv *top;
NaxWhitebox *whitebox;
vector<SimElement*> simElements;
//map<RvData, u64> pageFaultSinceFenceVma;

u64 statsStartAt = -1;
u64 statsStopAt = -1;
u64 statsToggleAt = -1;

#ifdef TRACE
VerilatedFstC* tfp;
#endif

VNaxRiscv_NaxRiscv *topInternal;
u64 traps_since_commit = 0;
u64 commits = 0;
u64 last_commit_pc;
int robIdChecked = 0;
int cycleSinceLastCommit = 0;
std::chrono::high_resolution_clock::time_point progressLast;
vluint64_t progressMainTimeLast = 0;

u64 simSlaveTraceDuration = 100000;
u64 simMasterTime = 0;
bool simMasterFailed = false;



void parseArgFirst(int argc, char** argv){
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
                traceWave = true;
#ifndef TRACE
                printf("You need to recompile with TRACE=yes to enable tracing"); failure();
#endif
            } break;
            case ARG_TRACE_START_TIME: trace_enable = false; addTimeEvent(stol(optarg), [&](){ trace_enable = true;}); break;
            case ARG_TRACE_STOP_TIME: trace_enable = false; addTimeEvent(stol(optarg), [&](){ trace_enable = false;}); break;
            case ARG_TRACE_SPORADIC: trace_enable = false; trace_sporadic_factor = stof(optarg); break;
            case ARG_TRACE_REF: trace_ref = true; break;
            case ARG_NAME: simName = optarg; break;
            case ARG_OUTPUT_DIR: outputDir = optarg; break;
            case ARG_TIMEOUT: timeout = stoi(optarg); break;
            case ARG_PROGRESS: progressPeriod = stod(optarg); break;
            case ARG_STATS_PRINT: statsPrint = true; break;
            case ARG_STATS_PRINT_ALL: statsPrint = true; statsPrintHist = true; break;
            case ARG_TRACE_GEM5: traceGem5 = true; break;
            case ARG_HELP: cout << helpString; exit(0); break;
            case ARG_SPIKE_DEBUG: spike_debug = true; break;
            case ARG_SIM_MASTER: simMaster = true; break;
            case ARG_SIM_SLAVE: simSlave = true; trace_enable = false; break;
            case ARG_SIM_SLAVE_DELAY: simSlaveTraceDuration = stol(optarg); break;
            case ARG_NO_PUTC_FLUSH: putcFlush = false;  break;
            case ARG_GETC: testScheduleQueue.push(new WaitPutc(string(optarg))); break;
            case ARG_PUTC: testScheduleQueue.push(new DoGetc(string(optarg))); break;
            case ARG_SUCCESS: testScheduleQueue.push(new DoSuccess()); break;
            case ARG_NO_STDIN: noStdIn = true; break;
            case ARG_MEMORY_LATENCY:
            case ARG_LOAD_HEX:
            case ARG_LOAD_ELF:
            case ARG_LOAD_BIN:
            case ARG_START_SYMBOL:
            case ARG_PASS_SYMBOL:
            case ARG_FAIL_SYMBOL:
            case ARG_STATS_START_SYMBOL:
            case ARG_STATS_STOP_SYMBOL:
            case ARG_STATS_TOGGLE_SYMBOL: break;
            default: {
                printf("Unknown argument\n");
                failure();
                break;
            }
        }
    }

    trace_sporadic_trigger = trace_sporadic_period * trace_sporadic_factor;
    mkpath(outputDir, 0777);
}

void parseArgsSecond(int argc, char** argv){
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
            case ARG_LOAD_HEX: wrap->memory.loadHex(string(optarg)); soc->memory.loadHex(string(optarg)); break;
            case ARG_LOAD_ELF: {
                elf = new Elf(optarg);
                elf->visitBytes([&](u8 data, u64 address) {
                    wrap->memory.write(address, 1, &data);
                    soc->memory.write(address, 1, &data);
                });
            }break;
            case ARG_LOAD_BIN: {
                u64 address;
                char path[201];
                if(sscanf(optarg, "%[^','],%lx", path, &address) == EOF) {
                    cout << "Bad load bin formating" << endl;
                    failure()
                }

                wrap->memory.loadBin(string(path), address);
                soc->memory.loadBin(string(path), address);
            }break;
            case ARG_START_SYMBOL: startPc = elf->getSymbolAddress(optarg); break;
            case ARG_PASS_SYMBOL: {
                u64 addr = elf->getSymbolAddress(optarg);
                addPcEvent(addr, [&](RvData pc){ success();});
                wrap->memory.write(addr, 4, nop);
                soc->memory.write(addr, 4, nop);
            }break;
            case ARG_FAIL_SYMBOL:  {
                u64 addr = elf->getSymbolAddress(optarg);
                addPcEvent(addr, [&](RvData pc){
                  printf("Failure due to fail symbol encounter\n");
                  failure();
                });
                wrap->memory.write(addr, 4, nop);
                soc->memory.write(addr, 4, nop);
            }break;
            case ARG_STATS_TOGGLE_SYMBOL: statsToggleAt = elf->getSymbolAddress(optarg); whitebox->statsCaptureEnable = false; break;
            case ARG_STATS_START_SYMBOL: statsStartAt = elf->getSymbolAddress(optarg); whitebox->statsCaptureEnable = false; break;
            case ARG_STATS_STOP_SYMBOL: statsStopAt = elf->getSymbolAddress(optarg); break;
            case ARG_MEMORY_LATENCY:{
                for(auto e : simElements){
                    if(auto v = dynamic_cast<WithMemoryLatency*>(e)) {
                       v->setLatency(stoi(optarg));
                    }
                }
            }break;
            default:  break;
        }
    }
    /* print all other parameters */
    while (optind < argc)
    {
        printf("other parameter: <%s>\n", argv[optind++]);
    }


    #ifdef TRACE
    if(traceWave){
        tfp = new VerilatedFstC;
        top->trace(tfp, 99);
        tfp->open((outputDir + "/wave.fst").c_str());
    }
    #endif


    state->pc = startPc;
    progressLast = std::chrono::high_resolution_clock::now();
}

void verilatorInit(int argc, char** argv){
    Verilated::debug(0);
    Verilated::randReset(2);
    Verilated::traceEverOn(true);
    Verilated::commandArgs(argc, argv);
    Verilated::mkdir("logs");
}

void spikeInit(){
    fptr = trace_ref ? fopen((outputDir + "/spike.log").c_str(),"w") : NULL;
    std::ofstream outfile ("/dev/null",std::ofstream::binary);
    wrap = new sim_wrap();

    proc = new processor_t("RV32IMA", "MSU", "", wrap, 0, false, fptr, outfile);
    if(trace_ref) proc->enable_log_commits();
    if(spike_debug) proc->debug = true;
    proc->set_pmp_num(0);
    state = proc->get_state();
}

void rtlInit(){
    top = new VNaxRiscv;  // Or use a const unique_ptr, or the VL_UNIQUE_PTR wrapper
    topInternal = top->NaxRiscv;

    whitebox = new NaxWhitebox(top->NaxRiscv);
    whitebox->traceGem5(traceGem5);
    if(traceGem5) whitebox->gem5 = ofstream(outputDir + "/trace.gem5o3",std::ofstream::binary);

    soc = new Soc(top);
    simElements.push_back(soc);
    simElements.push_back(new FetchCached(top, soc, true));
    simElements.push_back(new DataCached(top, soc, true));
    simElements.push_back(new LsuPeripheral(top, soc, &wrap->mmioDut, true));
    simElements.push_back(whitebox);
#ifdef ALLOCATOR_CHECKS
    simElements.push_back(new NaxAllocatorChecker(top->NaxRiscv));
#endif
}

void simMasterSlaveInit(){
    //https://www.geeksforgeeks.org/tcp-server-client-implementation-in-c/   <3

    if(simMaster){
        struct sockaddr_in servaddr, cli;
        socklen_t len;

        // socket create and verification
        syncSockfd = socket(AF_INET, SOCK_STREAM, 0);
        if (syncSockfd == -1) {
            printf("socket creation failed...\n");
            exit(0);
        }
        else
            printf("Socket successfully created..\n");
        bzero(&servaddr, sizeof(servaddr));

        // assign IP, PORT
        servaddr.sin_family = AF_INET;
        servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
        servaddr.sin_port = htons(SIM_MASTER_PORT);

        int one = 1;
        setsockopt(syncSockfd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));

        // Binding newly created socket to given IP and verification
        if ((bind(syncSockfd, (SA*)&servaddr, sizeof(servaddr))) != 0) {
            printf("socket bind failed...\n");
            exit(0);
        }
        else
            printf("Socket successfully binded..\n");

        // Now server is ready to listen and verification
        if ((listen(syncSockfd, 5)) != 0) {
            printf("Listen failed...\n");
            exit(0);
        }
        else
            printf("Server listening..\n");
        len = sizeof(cli);

        // Accept the data packet from client and verification
        syncConnfd = accept(syncSockfd, (SA*)&cli, &len);
        if (syncConnfd < 0) {
            printf("server accept failed...\n");
            exit(0);
        }
        else
            printf("server accept the client...\n");
    }

    if(simSlave){
        struct sockaddr_in servaddr, cli;
        socklen_t len;

        // socket create and varification
        syncSockfd = socket(AF_INET, SOCK_STREAM, 0);
        if (syncSockfd == -1) {
            printf("socket creation failed...\n");
            exit(0);
        }
        else
            printf("Socket successfully created..\n");
        bzero(&servaddr, sizeof(servaddr));

        // assign IP, PORT
        servaddr.sin_family = AF_INET;
        servaddr.sin_addr.s_addr = inet_addr("127.0.0.1");
        servaddr.sin_port = htons(SIM_MASTER_PORT);

        // connect the client socket to server socket
        if (connect(syncSockfd, (SA*)&servaddr, sizeof(servaddr)) != 0) {
            printf("connection with the server failed...\n");
            exit(0);
        }
        else
            printf("connected to the server..\n");
    }

}

void spikeStep(RobCtx & robCtx){
    //Sync some CSR
    state->mip->unlogged_write_with_mask(-1, 0);
    u64 backup;
    if(robCtx.csrReadDone){
        switch(robCtx.csrAddress){
        case MIP:
        case SIP:
        case UIP:
            backup = state->mie->read();
            state->mip->unlogged_write_with_mask(-1, robCtx.csrReadData);
            state->mie->unlogged_write_with_mask(MIE_MTIE | MIE_MEIE |  MIE_MSIE | MIE_SEIE, 0);
//                                cout << main_time << " " << hex << robCtx.csrReadData << " " << state->mip->read()  << " " << state->csrmap[robCtx.csrAddress]->read() << dec << endl;
            break;
        case CSR_MCYCLE:
            backup = state->minstret->read();
            state->minstret->unlogged_write(robCtx.csrReadData+1); //+1 patch a spike internal workaround XD
            break;
        case CSR_MCYCLEH:
            backup = state->minstret->read();
            state->minstret->unlogged_write((((u64)robCtx.csrReadData) << 32)+1);
            break;
        default:
            if(robCtx.csrAddress >= CSR_MHPMCOUNTER3 && robCtx.csrAddress <= CSR_MHPMCOUNTER31){
                state->csrmap[robCtx.csrAddress]->unlogged_write(robCtx.csrReadData);
            }
            break;
        }
    }

    //Run spike for one commit or trap
    proc->step(1);
    state->mip->unlogged_write_with_mask(-1, 0);

    //Sync back some CSR
    if(robCtx.csrReadDone){
        switch(robCtx.csrAddress){
        case MIP:
        case SIP:
        case UIP:
            state->mie->unlogged_write_with_mask(MIE_MTIE | MIE_MEIE |  MIE_MSIE | MIE_SEIE, backup);
            break;
        case CSR_MCYCLE:
        case CSR_MCYCLEH:
            state->minstret->unlogged_write(backup+2);
            break;
            break;
        }
    }
}

void spikeSyncTrap(){
    if(top->NaxRiscv->trap_fire){
        bool interrupt = top->NaxRiscv->trap_interrupt;
        int code = top->NaxRiscv->trap_code;
        bool pageFault = !interrupt && (code == 12 || code == 13 || code == 15);
        int mask = 1 << code;

        if(pageFault){
            auto mmu = proc->get_mmu();
            mmu->flush_tlb();
            mmu->fault_fetch = code == 12;
            mmu->fault_load  = code == 13;
            mmu->fault_store = code == 15;
            mmu->fault_address = top->NaxRiscv->trap_tval;
        }
        if(interrupt) state->mip->write_with_mask(mask, mask);
        proc->step(1);
        if(interrupt) state->mip->write_with_mask(mask, 0);
        if(pageFault){
            auto mmu = proc->get_mmu();
            mmu->fault_fetch = false;
            mmu->fault_load  = false;
            mmu->fault_store = false;
        }

        traps_since_commit += 1;
        if(traps_since_commit > 10){
            cout << "DUT is blocked in a endless trap cycle of death" << endl;
            failure();
        }
    }
}

#define simMasterWrite(buf) assert(write(syncConnfd, &buf, sizeof(buf)) == sizeof(buf));
#define simMasterRead(buf) assert(read(syncSockfd, &buf, sizeof(buf)) == sizeof(buf));

enum SIM_MS_ENUM
{
    SIM_MS_TIME = 1,
    SIM_MS_FAIL,
    SIM_MS_PASS,
    SIM_MS_GETC,
};

void simMasterWriteHeader(SIM_MS_ENUM e) {
    simMasterWrite(e);
}

void simMasterMainTime(){
    char buf[1+sizeof(main_time)];
    buf[0] = SIM_MS_TIME;
    simMasterWriteHeader(SIM_MS_TIME);
    simMasterWrite(main_time);
}

void simMasterGetC(char c){
    simMasterMainTime();
    simMasterWriteHeader(SIM_MS_GETC);
    simMasterWrite(c);
}

void simSlaveTick(){
    if(simMasterFailed) {
        if(main_time > simMasterTime){
            cout << "ERROR Slave sim is going futher than the master one ?????" << endl;
            failure();
        }
        return;
    }
    while(main_time + simSlaveTraceDuration >= simMasterTime){
        SIM_MS_ENUM header;
        simMasterRead(header);
        switch(header){
            case SIM_MS_TIME:{
                simMasterRead(simMasterTime);
            }break;
            case SIM_MS_FAIL:{
                simMasterFailed = true;
                auto time = simMasterTime - simSlaveTraceDuration;
                if(time > simMasterTime) time = main_time + 1;
                addTimeEvent(time, [&](){ trace_enable = true;});
                return;
            }break;
            case SIM_MS_PASS:{
                success();
            }break;
            case SIM_MS_GETC:{
                char c;
                simMasterRead(c);
                addTimeEvent(simMasterTime-1, [c](){ soc->customCin.push(c);});
            }break;
            default: {
                cout << "Unknown sim master header" << endl;
                failure();
            }break;
        }
    }
}


void simLoop(){
    try {
        top->clk = 0;

        for(SimElement* simElement : simElements) simElement->onReset();
        testScheduleQueueNext();
        while (!Verilated::gotFinish()) {
            if(simMaster && main_time % 50000 == 0){
                simMasterMainTime();
            }

            ++main_time;

            if(simSlave) simSlaveTick();

            if(main_time == timeout){
                printf("simulation timeout\n");
                failure();
            }
            if(timeToEvent.count(main_time) != 0){
                for(auto event : timeToEvent[main_time]){
                    event();
                }
            }
            if(trace_ref && proc->get_log_commits_enabled() != trace_enable){
                if(trace_enable){
                    proc->enable_log_commits();
                } else {
                    proc->disable_log_commits();
                }
            }

            #ifdef TRACE
            if(traceWave){
                if(trace_enable || (main_time % trace_sporadic_period) < trace_sporadic_trigger) tfp->dump(main_time);
                if(main_time % 100000 == 0) tfp->flush();
            }
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
                    if(CHECK_BIT(topInternal->commit_mask, i)){
                        cycleSinceLastCommit = 0;
                        int robId = topInternal->commit_robId + i;
                        auto &robCtx = whitebox->robCtx[robId];
                        robIdChecked = robId;
                        commits += 1;
                        traps_since_commit = 0;
    //                        printf("Commit %d %x\n", robId, whitebox->robCtx[robId].pc);

                        RvData pc = state->pc;
                        spikeStep(robCtx);

    //                        cout << state->minstret.get()->read() << endl;
                        last_commit_pc = pc;
                        assertEq("MISSMATCH PC", whitebox->robCtx[robId].pc,  pc);
                        for (auto item : state->log_reg_write) {
                            if (item.first == 0)
                              continue;

                            int rd = item.first >> 4;
                            switch (item.first & 0xf) {
                            case 0: { //integer
                                assertTrue("INTEGER WRITE MISSING", whitebox->robCtx[robId].integerWriteValid);
                                assertEq("INTEGER WRITE DATA", whitebox->robCtx[robId].integerWriteData, item.second.v[0]);
                            } break;
                            case 4:{ //CSR
                                u64 inst = state->last_inst.bits();
                                switch(inst){
                                case 0x30200073: //MRET
                                case 0x10200073: //SRET
                                case 0x00200073: //URET
                                    break;
                                default:
                                    assertTrue("CSR WRITE MISSING", whitebox->robCtx[robId].csrWriteDone);
                                    assertEq("CSR WRITE ADDRESS", whitebox->robCtx[robId].csrAddress & 0xCFF, rd & 0xCFF);
    //                                    assertEq("CSR WRITE DATA", whitebox->robCtx[robId].csrWriteData, item.second.v[0]);
                                    break;
                                }
                            } break;
                            default: {
                                printf("??? unknown spike trace");
                                failure();
                            } break;
                            }
                        }

                        if(pcToEvent.count(pc) != 0){
                            for(auto event : pcToEvent[pc]){
                                event(pc);
                            }
                        }

                        if(pc == statsToggleAt) {
                            whitebox->statsCaptureEnable = !whitebox->statsCaptureEnable;
                            cout << "Stats capture " << whitebox->statsCaptureEnable << " at " << main_time << endl;
                        }
                        if(pc == statsStartAt) whitebox->statsCaptureEnable = true;
                        if(pc == statsStopAt) whitebox->statsCaptureEnable = false;
                        whitebox->robCtx[robId].clear();
                    }
                }

                spikeSyncTrap();

                top->eval();
                for(SimElement* simElement : simElements) simElement->postCycle();
            }
        }
    }catch (const successException e) {
        printf("SUCCESS %s\n", simName.c_str());
        remove((outputDir + "/FAIL").c_str());
        auto f = fopen((outputDir + "/PASS").c_str(),"w");
        fclose(f);
        if(simMaster) simMasterWriteHeader(SIM_MS_PASS);
    } catch (const std::exception& e) {
        ++main_time;
        #ifdef TRACE
        if(traceWave){
        tfp->dump(main_time);
        if(main_time % 100000 == 0) tfp->flush();
        }
        #endif
        printf("TIME=%ld\n", main_time);
        printf("LAST PC COMMIT=%lx\n", last_commit_pc);
        printf("INCOMING SPIKE PC=%lx\n", state->pc);
        printf("ROB_ID=x%x\n", robIdChecked);
        printf("FAILURE %s\n", simName.c_str());
        remove((outputDir + "/PASS").c_str());
        auto f = fopen((outputDir + "/FAIL").c_str(),"w");
        fclose(f);
        if(simMaster) {
            simMasterMainTime();
            simMasterWriteHeader(SIM_MS_FAIL);
        }
    }

    if(statsPrint){
        printf("STATS :\n%s", whitebox->stats.report("  ", statsPrintHist).c_str());
    }
}

void cleanup(){
    if(fptr) {
        fflush(fptr);
        fclose(fptr);
    }

    #ifdef TRACE
    if(traceWave){
        tfp->flush();
        tfp->close();
    }
    #endif
    top->final();

    delete top;
    if(proc) delete proc;
    top = NULL;
    exit(0);
}

int main(int argc, char** argv, char** env){
    parseArgFirst(argc, argv);
    verilatorInit(argc, argv);
    spikeInit();
    rtlInit();
    parseArgsSecond(argc, argv);
    simMasterSlaveInit();
    simLoop();
    cleanup();
    return 0;
}
