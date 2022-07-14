#pragma once

#define REG_STATUS 0x0
#define REG_LOW 0x4
#define REG_HIGH 0x8
#define REG_SIZE 0xC
#define REG_DATA 0x10
#define REG_CAPACITY_LOW 0x20
#define REG_CAPACITY_HIGH 0x24

#include "string"
#include "type.h"
#include "stdio.h"

class SimpleBlockDevice : public SocElement{
public:
    string file;
    u64 capacity;
    u64 offset;
    u32 size;
    bool wr;
    u32 remain;
    FILE *fp;
    char *buffer;
    char *bufferPtr;
    bool hello;
    bool allowWrite;

    SimpleBlockDevice(char *path, bool allowWrite, u64 capacity, u64 mappingStart){
        hello = true;
        remain = 0;
        buffer = NULL;
        fp = fopen(path, "r+");
        this->mappingStart = mappingStart;
        this->capacity = capacity;
        this->mappingEnd = mappingStart + 0xFFF;
        this->allowWrite = allowWrite;
    }

    virtual ~SimpleBlockDevice(){
    }

    virtual int write(u64 address, uint32_t length, uint8_t *data){
        int err;
        if(hello) { printf("Block device access <3\n"); hello = false; }
        switch(address){
        case REG_LOW : memcpy(&offset, data, length); break;
        case REG_HIGH: memcpy(((char*)&offset)+4, data, length); break;
        case REG_SIZE: memcpy(&size, data, length); break;
        case REG_STATUS: {
            if((data[0] & 1) == 0){
                printf("??? simple block start ???\n");
                assert(false);
            }
            if(remain){
                printf("??? simple block remain ???\n");
                assert(false);
            }
            wr = (data[0] & 2) != 0;
            remain = size;
            if(wr && !allowWrite){
                printf("??? simple block write on read only ???\n");
                assert(false);
            }

            if(offset + size > capacity){
                printf("??? simple block out of range ???\n");
                assert(false);
            }
            if(fseek(fp, offset, SEEK_SET)){
                printf("??? simple block fgets ???\n");
                assert(false);
            }

//            printf("[SIM] SimpleBlockDevice: %lx %x %d\n", offset, size, wr);
            if(buffer) free(buffer);
            buffer = new char[size];
            bufferPtr = buffer;
            if(!wr){
                char* dummy = fgets(buffer, size, fp);
            }
            break;
        }
        case REG_DATA: {
            if(!remain){
                printf("??? simple block REG_DATA remain write ???\n");
                assert(false);
            }
            *bufferPtr++ = data[0];
            remain--;
            if(remain == 0){
                if(fputs(buffer, fp) < 0){
                    printf("??? simple block fputs ???\n");
                    assert(false);
                }
            }
        }break;
        default: return 1;break;
        }
        return 0;
    }

    virtual int read(u64 address, uint32_t length, uint8_t *data){
        if(hello) { printf("Block device access <3\n"); hello = false; }
        switch(address){
        case REG_STATUS: memset(data, 0, length); break;
        case REG_CAPACITY_LOW: memcpy(data, ((char*)&capacity)+0, length); break;
        case REG_CAPACITY_HIGH: memcpy(data, ((char*)&capacity)+4, length); break;
        case REG_DATA: {
            if(!remain){
                printf("??? simple block REG_DATA remain read ???\n");
                assert(false);
            }
            data[0] = *bufferPtr++;
            remain--;
        }break;
        default: return 1; break;
        }
        return 0;
    }


    virtual void onReset(){

    }

    virtual void postReset(){

    }


};
