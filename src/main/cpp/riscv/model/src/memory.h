// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

#pragma once

#include "type.h"
#include <stdio.h>
#include <stdint.h>
#include <iostream>

u32 hti(char c) {
	if (c >= 'A' && c <= 'F')
		return c - 'A' + 10;
	if (c >= 'a' && c <= 'f')
		return c - 'a' + 10;
	return c - '0';
}

u32 hToI(char *c, u32 size) {
	u32 value = 0;
	for (u32 i = 0; i < size; i++) {
		value += hti(c[i]) << ((size - i - 1) * 4);
	}
	return value;
}


class RandomGen{
public:
  u64 state = rand();
  void setSeed(u64 seed){
      state = seed;
  }

  u32 nextInt() {
    state = (state * 25214903917L + 11L) & 281474976710655L;
    return (u32)(state >> 16);
  }

  void nextBytes(u8* bytes, u64 len) {
    int randCnt = 0;
    u64 rand = 0l;
    for(u64 i = 0; i < len;i++){
      if(randCnt == 0) {
        rand = nextInt();
        randCnt = 4;
      }
      bytes[i] = (u8) rand;
      rand >>= 8;
      randCnt -=1;
    }
  }
};


class Memory{
public:
	u8* mem[1 << 12];
	u64 seed = 0;
	u64 randOffset = 0;

	Memory(){
		for(u32 i = 0;i < (1 << 12);i++) mem[i] = NULL;
	}

	~Memory(){
		for(u32 i = 0;i < (1 << 12);i++) if(mem[i]) delete [] mem[i];
	}

	u8* get(u32 address){
	    u32 blockId = address >> 20;
		if(mem[blockId] == NULL) {
			u8* ptr = new u8[1024*1024];
			RandomGen rand;
			rand.setSeed(seed ^ ((blockId << 20) + randOffset));
			rand.nextBytes(ptr, 1024*1024);
			mem[blockId] = ptr;
		}
		return &mem[blockId][address & 0xFFFFF];
	}

	void read(u32 address,u32 length, u8 *data){
		for(u32 i = 0;i < length;i++){
			data[i] = (*this)[address + i];
		}
	}

	void write(u32 address,u32 length, u8 *data){
		for(u32 i = 0;i < length;i++){
			(*this)[address + i] = data[i];
		}
	}

	u8& operator [](u32 address) {
		return *get(address);
	}

	/*T operator [](u32 address) const {
		return get(address);
	}*/

	void loadHex(std::string path) {
    	FILE *fp = fopen(&path[0], "r");
    	if(fp == 0){
    	    std::cout << path << " not found" << std::endl;
    		throw std::exception();
    	}

    	fseek(fp, 0, SEEK_END);
    	u32 size = ftell(fp);
    	fseek(fp, 0, SEEK_SET);
    	char* content = new char[size];
    	if (fread(content, 1, size, fp)){}
    	fclose(fp);

    	int offset = 0;
    	char* line = content;
    	while (1) {
    		if (line[0] == ':') {
    			u32 byteCount = hToI(line + 1, 2);
    			u32 nextAddr = hToI(line + 3, 4) + offset;
    			u32 key = hToI(line + 7, 2);
    //			printf("%d %d %d\n", byteCount, nextAddr,key);
    			switch (key) {
    			case 0:
    				for (u32 i = 0; i < byteCount; i++) {
    					*(this->get(nextAddr + i)) = hToI(line + 9 + i * 2, 2);
    					//printf("%x %x %c%c\n",nextAddr + i,hToI(line + 9 + i*2,2),line[9 + i * 2],line[9 + i * 2+1]);
    				}
    				break;
    			case 2:
    //				cout << offset << endl;
    				offset = hToI(line + 9, 4) << 4;
    				break;
    			case 4:
    //				cout << offset << endl;
    				offset = hToI(line + 9, 4) << 16;
    				break;
    			default:
    //				cout << "??? " << key << endl;
    				break;
    			}
    		}

    		while (*line != '\n' && size != 0) {
    			line++;
    			size--;
    		}
    		if (size <= 1)
    			break;
    		line++;
    		size--;
    	}

    	delete [] content;
    }

    void loadBin(std::string path, uint64_t offset) {
    	FILE *fp = std::fopen(&path[0], "r");
    	if(fp == 0){
    	    std::cout << path << " not found" << std::endl;
    		throw std::exception();
    	}

    	fseek(fp, 0, SEEK_END);
    	u32 size = ftell(fp);
    	fseek(fp, 0, SEEK_SET);
    	char* content = new char[size];
    	auto miaou = fread(content, 1, size, fp);
    	fclose(fp);

    	for(u32 byteId = 0; byteId < size;byteId++){
    		*(this->get(offset + byteId)) = content[byteId];
    	}

    	delete [] content;
    }
};

