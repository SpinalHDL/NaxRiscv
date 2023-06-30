// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

#pragma once

uint32_t hti(char c) {
	if (c >= 'A' && c <= 'F')
		return c - 'A' + 10;
	if (c >= 'a' && c <= 'f')
		return c - 'a' + 10;
	return c - '0';
}

uint32_t hToI(char *c, uint32_t size) {
	uint32_t value = 0;
	for (uint32_t i = 0; i < size; i++) {
		value += hti(c[i]) << ((size - i - 1) * 4);
	}
	return value;
}




class Memory{
public:
	uint8_t* mem[1 << 12];

	Memory(){
		for(uint32_t i = 0;i < (1 << 12);i++) mem[i] = NULL;
	}
	~Memory(){
		for(uint32_t i = 0;i < (1 << 12);i++) if(mem[i]) delete [] mem[i];
	}

	uint8_t* get(uint32_t address){
		if(mem[address >> 20] == NULL) {
			uint8_t* ptr = new uint8_t[1024*1024];
			for(uint32_t i = 0;i < 1024*1024;i+=4) {
				ptr[i + 0] = 0xFF;
				ptr[i + 1] = 0xFF;
				ptr[i + 2] = 0xFF;
				ptr[i + 3] = 0xFF;
			}
			mem[address >> 20] = ptr;
		}
		return &mem[address >> 20][address & 0xFFFFF];
	}

	void read(uint32_t address,uint32_t length, uint8_t *data){
		for(int i = 0;i < length;i++){
			data[i] = (*this)[address + i];
		}
	}

	void write(uint32_t address,uint32_t length, uint8_t *data){
		for(int i = 0;i < length;i++){
			(*this)[address + i] = data[i];
		}
	}

	uint8_t& operator [](uint32_t address) {
		return *get(address);
	}

	/*T operator [](uint32_t address) const {
		return get(address);
	}*/

	void loadHex(string path) {
    	FILE *fp = fopen(&path[0], "r");
    	if(fp == 0){
    		cout << path << " not found" << endl;
    		throw exception();
    	}

    	fseek(fp, 0, SEEK_END);
    	uint32_t size = ftell(fp);
    	fseek(fp, 0, SEEK_SET);
    	char* content = new char[size];
    	if (fread(content, 1, size, fp));
    	fclose(fp);

    	int offset = 0;
    	char* line = content;
    	while (1) {
    		if (line[0] == ':') {
    			uint32_t byteCount = hToI(line + 1, 2);
    			uint32_t nextAddr = hToI(line + 3, 4) + offset;
    			uint32_t key = hToI(line + 7, 2);
    //			printf("%d %d %d\n", byteCount, nextAddr,key);
    			switch (key) {
    			case 0:
    				for (uint32_t i = 0; i < byteCount; i++) {
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

    void loadBin(string path, uint64_t offset) {
    	FILE *fp = fopen(&path[0], "r");
    	if(fp == 0){
    		cout << path << " not found" << endl;
    		throw exception();
    	}

    	fseek(fp, 0, SEEK_END);
    	uint32_t size = ftell(fp);
    	fseek(fp, 0, SEEK_SET);
    	char* content = new char[size];
    	auto miaou = fread(content, 1, size, fp);
    	fclose(fp);

    	for(int byteId = 0; byteId < size;byteId++){
    		*(this->get(offset + byteId)) = content[byteId];
    	}

    	delete [] content;
    }
};

