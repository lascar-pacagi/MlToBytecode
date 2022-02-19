#ifndef _VM_HPP_
#define _VM_HPP_
#include <vector>
#include <iostream>
#include <string>

class vm {
    enum opcode : uint8_t {
        STOP,
        DUPL,
        SWAP,
        ROT3,
        IROT3,
        FST,
        SND,
        SET_FST,
        SET_SND,
        CONS,
        SPLIT,
        ADD,
        SUB,
        MUL,   
        EQ,
        LT,
        CALL,
        RETURN,
        BRANCH,
        JUMP,
        LOAD,         
        PUSH_NIL,
        PUSH,
        LEN,
    };
    static constexpr int moffset = 100000000;
    std::vector<int> data_memory;
    int mc = 1;
    std::vector<int> stack;
    int sp = 0;
    int max_sp = 0;
    std::vector<uint8_t> instruction_memory;
    int pc = 0;

    int get_int32_little_endian(const uint8_t*const memory) const {
        int res = memory[0];
        res += memory[1] << 8;
        res += memory[2] << 16;
        res += memory[3] << 24;
        return res;
    }
    int eaddrm(int addr) const {
        return (addr + moffset) << 1;
    }
    int daddrm(int addr) const {
        return (addr >> 1) - moffset;
    }
    int eaddri(int addr) const {
        return addr << 1;
    }
    int daddri(int addr) const {
        return addr >> 1;
    }
    int eint(int i) const {
        return i << 1 | 1;
    }
    int dint(int i) const {
        return i >> 1;
    }
    bool is_addri(int a) const {
        return (a & 1) == 0 && a >> 1 < moffset;
    }
    bool is_addrm(int a) const {
        return (a & 1) == 0 && a >> 1 >= moffset;
    }
    bool is_value(int i) const {
        return i & 1;
    }    
    bool structural_eq(int v1, int v2) const;
    int structural_cmp(int v1, int v2) const;
    bool structural_lt(int v1, int v2) const;
    void print_program(std::ostream&) const;
    void print_stack(std::ostream&) const;
    void print_memory(std::ostream&) const;
    void print_value(std::ostream&, int v) const;
public:
    vm(size_t dsize, size_t ssize, const std::string& program);
    void exec();
    friend std::ostream& operator<<(std::ostream& os, const vm& vm);
};
#endif