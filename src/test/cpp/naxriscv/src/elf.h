#pragma once

#include "stdio.h"
#include "type.h"
#include <stdexcept>
#include <byteswap.h>
//
#include <iostream>
#include <elfio/elfio.hpp>
#include <elf.h>

using namespace ELFIO;

//https://chris.bracken.jp/2018/10/decoding-an-elf-binary/ <3
class Elf{
public:
    elfio reader;
    Elf(char* path){
        if ( !reader.load( path )) {
            std::cout << "Can't find or process ELF file " << path << std::endl;
            exit(1);
        }
    }

    void visitBytes(std::function<void(u8, u64)> func){
        // Print ELF file sections info
        Elf_Half sec_num = reader.sections.size();
        for (int i = 0; i < sec_num; ++i) {
            section *psec = reader.sections[i];
            Elf_Xword flags = psec->get_flags();
            if(flags & SHF_ALLOC){
                auto size = psec->get_size();
                auto data = reader.sections[i]->get_data();
                u64 address = psec->get_address();
                if(data) for(int i = 0;i < size; i++){
                    func(*data++, address++);
                }
            }
        }
    }

    u64 getSymbolAddress(char *targetName){
        Elf_Half sec_num = reader.sections.size();
        for (int i = 0; i < sec_num; ++i) {
            section *psec = reader.sections[i];
            // Check section type
            if (psec->get_type() == SHT_SYMTAB) {
                const symbol_section_accessor symbols(reader, psec);
                for (unsigned int j = 0; j < symbols.get_symbols_num(); ++j) {
                    std::string name;
                    Elf64_Addr value;
                    Elf_Xword size;
                    unsigned char bind;
                    unsigned char type;
                    Elf_Half section_index;
                    unsigned char other;

                    // Read symbol properties
                    symbols.get_symbol(j, name, value, size, bind, type,
                            section_index, other);
                    if(!strcmp(targetName, name.c_str())){
                        return value;
                    }
                }
            }
        }
        std::cout << "Can't find symbole " << targetName << std::endl;
        exit(1);
    }
};
