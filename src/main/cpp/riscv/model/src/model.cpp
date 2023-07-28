#include <stdint.h>
#include <string>
#include <memory>
#include <jni.h>
#include <iostream>
#include <queue>
#include <sstream>

#include "processor.h"
#include "mmu.h"
#include "simif.h"
#include "type.h"
#include "memory.h"
#include "elf.h"

using namespace std;



#include <stdio.h>
#include <stdint.h>


#define CSR_UCYCLE 0xC00
#define CSR_UCYCLEH 0xC80
#define MIP 0x344
#define SIP 0x144
#define UIP  0x44
#define CAUSE_MACHINE_SOFTWARE 3
#define CAUSE_MACHINE_TIMER 7
#define CAUSE_MACHINE_EXTERNAL 11
#define CAUSE_SUPERVISOR_EXTERNAL 9
#define MIE_MTIE (1 << CAUSE_MACHINE_TIMER)
#define MIE_MEIE (1 << CAUSE_MACHINE_EXTERNAL)
#define MIE_MSIE (1 << CAUSE_MACHINE_SOFTWARE)
#define MIE_SEIE (1 << CAUSE_SUPERVISOR_EXTERNAL)


#define API __attribute__((visibility("default")))

class successException : public std::exception { };
#define failure() throw std::exception();
#define success() throw successException();
void breakMe(){
    volatile int a = 0;
}

#define assertEq(message, x,ref) if((x) != (ref)) {\
    cout << hex << "\n*** " << message << " DUT=" << x << " REF=" << ref << " ***\n\n" << dec;\
    breakMe();\
    failure();\
}

#define assertTrue(message, x) if(!(x)) {\
    printf("\n*** %s ***\n\n",message);\
    breakMe();\
    failure();\
}


class TraceIo{
public:
    bool write;
    u64 address;
    u64 data;
    u32 mask;
    u32 size;
    bool error;

    TraceIo(std::istringstream &f){
        f >> write >> hex >> address >> data >> mask >> dec >> size >> error;
    }
};


class SpikeIf : public simif_t{
public:
    Memory *memory;
    queue <TraceIo> ioQueue;

    SpikeIf(Memory *memory){
        this->memory = memory;
    }

    // should return NULL for MMIO addresses
    virtual char* addr_to_mem(reg_t addr)  {
//        if((addr & 0xE0000000) == 0x00000000) return NULL;
//        printf("addr_to_mem %lx ", addr);
//        return (char*) memory->get(addr);
        return NULL;
    }
    // used for MMIO addresses
    virtual bool mmio_load(reg_t addr, size_t len, u8* bytes)  {
        if((addr & 0xE0000000) != 0x00000000) {
            memory->read(addr, len, bytes);
            return true;
        }
//        printf("mmio_load %lx %ld\n", addr, len);
        if(addr < 0x10000000 || addr > 0x20000000) return false;
        assertTrue("missing mmio\n", !ioQueue.empty());
        auto dut = ioQueue.front();
        assertEq("mmio write\n", dut.write, false);
        assertEq("mmio address\n", dut.address, addr);
        assertEq("mmio len\n", dut.size, len);
        memcpy(bytes, (u8*)&dut.data, len);
        ioQueue.pop();
        return !dut.error;
    }
    virtual bool mmio_store(reg_t addr, size_t len, const u8* bytes)  {
        if((addr & 0xE0000000) != 0x00000000) {
            memory->write(addr, len, (u8*) bytes);
            return true;
        }
//        printf("mmio_store %lx %ld\n", addr, len);
        if(addr < 0x10000000 || addr > 0x20000000) return false;
        assertTrue("missing mmio\n", !ioQueue.empty());
        auto dut = ioQueue.front();
        assertEq("mmio write\n", dut.write, true);
        assertEq("mmio address\n", dut.address, addr);
        assertEq("mmio len\n", dut.size, len);
        assertTrue("mmio data\n", !memcmp((u8*)&dut.data, bytes, len));
        ioQueue.pop();
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

class Hart{
public:
    SpikeIf *sif;
    processor_t *proc;
    state_t *state;

    u32 physWidth;

    bool integerWriteValid;
    u64 integerWriteData;

    u32 csrAddress;
    bool csrWrite;
    bool csrRead;
    u64 csrWriteData;
    u64 csrReadData;


    Hart(u32 hartId, string isa, string priv, u32 physWidth, Memory *memory){
        this->physWidth = physWidth;
        sif = new SpikeIf(memory);
        FILE *fptr = 1 ? fopen(string("spike.log").c_str(),"w") : NULL;
        std::ofstream outfile ("/dev/null",std::ofstream::binary);
        proc = new processor_t(isa.c_str(), priv.c_str(), "", sif, hartId, false, fptr, outfile);
        auto xlen = proc->get_xlen();
        proc->set_impl(IMPL_MMU_SV32, xlen == 32);
        proc->set_impl(IMPL_MMU_SV39, xlen == 64);
        proc->set_impl(IMPL_MMU_SV48, false);
        proc->set_impl(IMPL_MMU, true);
        proc->enable_log_commits();
        proc->set_pmp_num(0);
        proc->debug = true;
        state = proc->get_state();
        state->csrmap[CSR_MCYCLE] = std::make_shared<basic_csr_t>(proc, CSR_MCYCLE, 0);
        state->csrmap[CSR_MCYCLEH] = std::make_shared<basic_csr_t>(proc, CSR_MCYCLEH, 0);
    }

//    void syncTime(u64 time){
//        state->csrmap[CSR_MCYCLE]->unlogged_write(time);
//        if(proc->get_xlen() == 32){
//            state->csrmap[CSR_MCYCLEH]->unlogged_write(time >> 32);
//        }
//    }

    void close() {
        auto f = proc->get_log_file();
        if(f) fclose(f);
    }

    void setPc(u64 pc){
        state->pc = pc;
    }

    void writeRf(u32 rfKind, u32 address, u64 data){
        switch(rfKind){
        case 0:
            integerWriteValid = true;
            integerWriteData = data;
            break;
        case 4:
            if((csrWrite || csrRead) && csrAddress != address){
                printf("duplicated CSR access \n");
                failure();
            }
            csrAddress = address;
            csrWrite = true;
            csrWriteData = data;
            break;
        default:
            printf("??? unknown RF trace \n");
            failure();
            break;
        }

    }


    void readRf(u32 rfKind, u32 address, u64 data){
        switch(rfKind){
        case 4:
            if((csrWrite || csrRead) && csrAddress != address){
                printf("duplicated CSR access \n");
                failure();
            }
            csrAddress = address;
            csrRead = true;
            csrReadData = data;
            break;
        default:
            printf("??? unknown RF trace \n");
            failure();
            break;
        }

    }

    void physExtends(u64 &v){
        v = (u64)(((s64)v<<(64-physWidth)) >> (64-physWidth));
    }

    void trap(bool interrupt, u32 code){
        int mask = 1 << code;
        auto fromPc = state->pc;
        if(interrupt) state->mip->write_with_mask(mask, mask);
        proc->step(1);
        if(interrupt) state->mip->write_with_mask(mask, 0);
        if(!state->trap_happened){
            printf("DUT did trap on %lx\n", fromPc);
            failure();
        }
        assertEq("DUT interrupt missmatch", interrupt, state->trap_interrupt);
        assertEq("DUT code missmatch", code, state->trap_code);
        physExtends(state->pc);
    }

    void commit(u64 pc){
        if(pc != state->pc){
            printf("PC MISSMATCH dut=%lx ref=%lx\n", pc, state->pc);
            failure();
        }

        //Sync CSR
        u64 csrBackup = 0;
        if(csrRead){
            switch(csrAddress){
            case CSR_MCYCLE:
            case CSR_MCYCLEH:
            case CSR_UCYCLE:
            case CSR_UCYCLEH:
                state->csrmap[csrAddress]->unlogged_write(csrReadData);
                break;
            case MIP:
            case SIP:
            case UIP:
                csrBackup = state->mie->read();
                state->mip->unlogged_write_with_mask(-1, csrReadData);
                state->mie->unlogged_write_with_mask(MIE_MTIE | MIE_MEIE |  MIE_MSIE | MIE_SEIE, 0);
    //                                cout << main_time << " " << hex << robCtx.csrReadData << " " << state->mip->read()  << " " << state->csrmap[robCtx.csrAddress]->read() << dec << endl;
                break;
            }
            if(csrAddress >= CSR_MHPMCOUNTER3 && csrAddress <= CSR_MHPMCOUNTER31){
                state->csrmap[csrAddress]->unlogged_write(csrReadData);
            }
        }

        //Run the spike model
        proc->step(1);

        //Sync back some CSR
        state->mip->unlogged_write_with_mask(-1, 0);
        if(csrRead){
            switch(csrAddress){
            case MIP:
            case SIP:
            case UIP:
                state->mie->unlogged_write_with_mask(MIE_MTIE | MIE_MEIE |  MIE_MSIE | MIE_SEIE, csrBackup);
                break;
            }
        }

        //Checks
        printf("%016lx %08lx\n", pc, state->last_inst.bits());
        assertTrue("DUT missed a trap", !state->trap_happened);
        for (auto item : state->log_reg_write) {
            if (item.first == 0)
              continue;

            u32 rd = item.first >> 4;
            switch (item.first & 0xf) {
            case 0: { //integer
                assertTrue("INTEGER WRITE MISSING", integerWriteValid);
                assertEq("INTEGER WRITE MISSMATCH", integerWriteData, item.second.v[0]);
                integerWriteValid = false;
            } break;
            case 4:{ //CSR
                u64 inst = state->last_inst.bits();
                switch(inst){
                case 0x30200073: //MRET
                case 0x10200073: //SRET
                case 0x00200073: //URET
                    physExtends(state->pc);
                    break;
                default:{
                    if((inst & 0x7F) == 0x73 && (inst & 0x3000) != 0){
                        assertTrue("CSR WRITE MISSING", csrWrite);
                        assertEq("CSR WRITE ADDRESS", (u32)(csrAddress & 0xCFF), (u32)(rd & 0xCFF));
//                                                assertEq("CSR WRITE DATA", whitebox->robCtx[robId].csrWriteData, item.second.v[0]);
                    }
                    break;
                }

                }
                csrWrite = false;
            } break;
            default: {
                printf("??? unknown spike trace %lx\n", item.first & 0xf);
                failure();
            } break;
            }
        }

        csrRead = false;
        assertTrue("CSR WRITE SPAWNED", !csrWrite);
        assertTrue("INTEGER WRITE SPAWNED", !integerWriteValid);
    }

    void ioAccess(TraceIo io){
        sif->ioQueue.push(io);
    }

    void setInt(u32 id, bool value){

    }

};

class Context{
public:
    Memory memory;
    vector<Hart*> harts;

    void loadElf(string path, u64 offset){
        auto elf = new Elf(path.c_str());
        elf->visitBytes([&](u8 data, u64 address) {
            memory.write(address+offset, 1, &data);
        });
    }

    void rvNew(u32 hartId, string isa, string priv, u32 physWidth){
        harts.resize(max((size_t)(hartId+1), harts.size()));
        harts[hartId] = new Hart(hartId, isa, priv, physWidth, &memory);
    }

    void close(){
        for(auto hart : harts){
            if(hart) hart->close();
        }
    }
};


#ifdef __cplusplus
extern "C" {
#endif
//jclass userDataClass;
//jmethodID methodId;
//
//JNIEXPORT jlong JNICALL Java_riscv_model_Model_newModel
//  (JNIEnv * env, jobject obj){
//
//    auto * model = new Model();
//    return (jlong)model;
//}
//
//JNIEXPORT void JNICALL Java_riscv_model_Model_deleteModel
//  (JNIEnv * env, jobject obj, jlong model){
//
//    delete (Model*)model;
//}


#ifdef __cplusplus
}
#endif




void checkFile(std::ifstream &lines){
    Context context;
#define rv context.harts[hartId]
    std::string line;
    u64 lineId = 0;
    try{
        while (getline(lines, line)){
            istringstream f(line);
            string str;
            f >> str;
            if(str == "rv"){
                f >> str;
                if (str == "commit") {
                    u32 hartId;
                    u64 pc;
                    f >> hartId >> hex >> pc >> dec;
                    rv->commit(pc);
                } else if (str == "rf") {
                    f >> str;
                    if(str == "w") {
                        u32 hartId, rfKind, address;
                        u64 data;
                        f >> hartId >> rfKind >> address >> hex >> data >> dec;
                        rv->writeRf(rfKind, address, data);
                    } else if(str == "r") {
                        u32 hartId, rfKind, address;
                        u64 data;
                        f >> hartId >> rfKind >> address >> hex >> data >> dec;
                        rv->readRf(rfKind, address, data);
                    } else {
                        throw runtime_error(line);
                    }

                } else if (str == "io") {
                    u32 hartId;
                    f >> hartId;
                    auto io = TraceIo(f);
                    rv->ioAccess(io);
                } else if (str == "trap") {
                    u32 hartId, code;
                    bool interrupt;
                    f >> hartId >> interrupt >> code;
                    rv->trap(interrupt, code);
                } else if (str == "int") {
                    f >> str;
                    if(str == "set") {
                        u32 hartId, intId;
                        bool value;
                        f >> hartId >> intId >> value;
                        rv->setInt(intId, value);
                    } else {
                        throw runtime_error(line);
                    }
                } else if (str == "set") {
                    f >> str;
                    if(str == "pc"){
                        u32 hartId;
                        u64 pc;
                        f >> hartId >> hex >> pc >> dec;
                        rv->setPc(pc);
                    } else {
                        throw runtime_error(line);
                    }
                } else if(str == "new"){
                    u32 hartId, physWidth;
                    string isa, priv;
                    f >> hartId >> isa >> priv >> physWidth;
                    context.rvNew(hartId, isa, priv, physWidth);
                } else {
                    throw runtime_error(line);
                }
            } else if(str == "elf"){
                f >> str;
                if(str == "load"){
                    string path;
                    u64 offset;
                    f >> path >> hex >> offset >> dec;
                    context.loadElf(path, offset);
                } else {
                    throw runtime_error(line);
                }
            } else {
                throw runtime_error(line);
            }
            lineId += 1;
        }
    } catch (const std::exception &e) {
        printf("Failed at line %ld : %s\n", lineId, line.c_str());
        context.close();
        throw e;
    }
    context.close();

    cout << "Model check Success <3" << endl;
}


int main(){
    cout << "miaou3" << endl;
    std::ifstream f("/media/data/open/riscv/VexRiscvOoo/trace.txt");
    checkFile(f);
}

