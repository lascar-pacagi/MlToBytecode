#include "vm.hpp"
#include <stdexcept>
#include <fstream>
#include <iomanip>

using namespace std;

vm::vm(size_t dsize, size_t ssize, const string& program) 
    : data_memory(dsize), stack(ssize) {
    ifstream is(program, ios_base::binary);
    is.exceptions(ifstream::failbit | ifstream::badbit);
    is.seekg(0, is.end);
    int n = is.tellg();
    is.seekg(0, is.beg);        
    instruction_memory.resize(n);
    is.read(reinterpret_cast<char*>(instruction_memory.data()), n);  
    print_program(cerr);
    cerr << '\n';
    stack[0] = eaddrm(0);
}

void vm::exec() {    
    while (true) {  
        switch (instruction_memory[pc]) {
            case STOP: return;
            case DUPL:
                stack[sp + 1] = stack[sp];
                sp++;
                break;
            case SWAP:            
                swap(stack[sp], stack[sp - 1]);
                break;
            case ROT3:
                rotate(begin(stack) + sp - 2, begin(stack) + sp, begin(stack) + sp + 1);
                break;
            case IROT3:
                rotate(begin(stack) + sp - 2, begin(stack) + sp - 1, begin(stack) + sp + 1);
                break;
            case FST:
                stack[sp] = data_memory[daddrm(stack[sp])];
                break;
            case SND:
                stack[sp] = data_memory[daddrm(stack[sp]) + 1];
                break;
            case SET_FST:
                data_memory[daddrm(stack[sp - 1])] = stack[sp];
                sp--;
                break;
            case SET_SND:
                data_memory[daddrm(stack[sp - 1]) + 1] = stack[sp];
                sp--;
                break;
            case CONS:
                data_memory[mc]     = stack[sp];
                data_memory[mc + 1] = stack[sp - 1];
                sp--;
                stack[sp] = eaddrm(mc);
                mc += 2;
                break;
            case SPLIT:                
                stack[sp + 1] = data_memory[daddrm(stack[sp])];
                stack[sp]     = data_memory[daddrm(stack[sp]) + 1];
                sp++;
                break;
            case ADD:
                stack[sp - 1] = eint(dint(stack[sp]) + dint(stack[sp - 1]));
                sp--;
                break;
            case SUB:
                stack[sp - 1] = eint(dint(stack[sp]) - dint(stack[sp - 1]));
                sp--;
                break;
            case MUL: 
                stack[sp - 1] = eint(dint(stack[sp]) * dint(stack[sp - 1]));
                sp--;
                break;  
            case EQ:
                stack[sp - 1] = eint(structural_eq(stack[sp], stack[sp - 1]));                
                sp--;
                break;
            case LT:
                stack[sp - 1] = eint(structural_lt(stack[sp], stack[sp - 1]));                
                sp--;
                break;  
            case CALL: {
                int f = daddri(stack[sp]);
                stack[sp] = stack[sp - 1];
                stack[sp - 1] = eaddri(pc + 1);
                pc = f - 1;
                break;
            }                
            case RETURN:                
                pc = daddri(stack[sp - 1]) - 1;
                stack[sp - 1] = stack[sp];
                sp--;
                break;
            case BRANCH:
                if (dint(stack[sp])) {
                    stack[sp] = get_int32_little_endian(&instruction_memory[pc + 1]);
                } else {
                    stack[sp] = get_int32_little_endian(&instruction_memory[pc + 1 + 4]);
                }
                pc += 8;                
                break;
            case JUMP:
                pc = daddri(get_int32_little_endian(&instruction_memory[pc + 1])) - 1;
                break;                
            case LOAD:
                stack[sp] = get_int32_little_endian(&instruction_memory[pc + 1]);
                pc += 4;
                break;
            case PUSH_NIL: 
                sp++;
                stack[sp] = eaddrm(0);
                break;            
            case PUSH:
                sp++;
                stack[sp] = get_int32_little_endian(&instruction_memory[pc + 1]);
                pc += 4;
                break;            
            default: throw invalid_argument("invalid opcode");
        }
        max_sp = max(max_sp, sp);
        pc++;
    }
}

int vm::structural_cmp(int v1, int v2) const {
    if (is_value(v1) && is_value(v2)) {
        int i1 = dint(v1);
        int i2 = dint(v2);
        if (i1 < i2) return -1;
        if (i1 == i2) return 0;
        return 1;        
    }
    if (is_addrm(v1) && is_addrm(v2)) {
        int a1 = daddrm(v1);
        int a2 = daddrm(v2);
        if (a1 == 0 && a2 == 0) return 0;
        if (a1 == 0 || a2 == 0) throw invalid_argument("not comparable");
        int cmp1 = structural_cmp(data_memory[a1], data_memory[a2]);
        int cmp2 = structural_cmp(data_memory[a1 + 1], data_memory[a2 + 1]);
        if (cmp1 == 0) return cmp2;
        return cmp1;        
    }
    throw invalid_argument("not comparable");
}

bool vm::structural_eq(int v1, int v2) const {
    try {
        return structural_cmp(v1, v2) == 0;   
    } catch (...) {
        return false;
    }
}

bool vm::structural_lt(int v1, int v2) const {
    try {
        return structural_cmp(v1, v2) == -1;   
    } catch (...) {
        return false;
    }    
}

ostream& operator<<(ostream& os, const vm& vm) {
    vm.print_value(os, vm.stack[vm.sp]);
    os << '\n';
    os << "program size : " << setw(4) << vm.instruction_memory.size() << " bytes\n";
    os << "memory needed: " << setw(4) << 4 * vm.mc << " bytes\n";
    os << "stack needed : " << setw(4) << 4 * (vm.max_sp + 1) << " bytes";
    return os;
}

void vm::print_value(ostream& os, int v) const {
    if (is_addrm(v)) {
        int a = daddrm(v);
        if (a == 0) { 
            os << "nil";
            return;
        }
        os << '(';
        print_value(os, data_memory[a]);
        os << ", ";
        print_value(os, data_memory[a + 1]);
        os << ')';
        return;
    }
    if (is_addri(v)) {
        os << "<fun>";
        return;
    }
    os << dint(v);
}

void vm::print_program(ostream& os) const {
    int pc = 0;
    constexpr int w = 5;
    while (true) {
        os << setw(3) << pc << ": ";
        switch (instruction_memory[pc]) {
            case STOP: 
                os << "stop";
                return;
            case DUPL:
                os << "dupl\n";
                break;
            case SWAP:            
                os << "swap\n";
                break;
            case ROT3:
                os << "rot3\n";
                break;
            case IROT3:
                os << "irot3\n";
                break;
            case FST:
                os << "fst\n";
                break;
            case SND:
                os << "snd\n";
                break;
            case SET_FST:
                os << "set_fst\n";
                break;
            case SET_SND:
                os << "set_snd\n";
                break;
            case CONS:
                os << "cons\n";
                break;
            case SPLIT:
                os << "split\n";
                break;
            case ADD:
                os << "add\n";
                break;
            case SUB:
                os << "sub\n";
                break;
            case MUL: 
                os << "mul\n";
                break;  
            case EQ:
                os << "eq\n";
                break;
            case LT:
                os << "lt\n";
                break;  
            case CALL:
                os << "call\n";
                break;       
            case RETURN:
                os << "return\n";
                break;
            case BRANCH:
                os << "branch";
                os << setw(w) << get_int32_little_endian(&instruction_memory[pc + 1]);
                os << setw(w) << get_int32_little_endian(&instruction_memory[pc + 1 + 4]) << '\n';
                pc += 8;                
                break;
            case JUMP:
                os << "jump";
                os << setw(w) << get_int32_little_endian(&instruction_memory[pc + 1]) << '\n';    
                pc += 4;
                break;                
            case LOAD:
                os << "load";
                os << setw(w) << get_int32_little_endian(&instruction_memory[pc + 1]) << '\n';
                pc += 4;
                break;
            case PUSH_NIL: 
                os << "push_nil\n";
                break;            
            case PUSH:
                os << "push";
                os << setw(w) << get_int32_little_endian(&instruction_memory[pc + 1]) << '\n';
                pc += 4;
                break;            
            default: throw invalid_argument("invalid opcode");          
        }
        pc++;
    }
}

void vm::print_stack(ostream& os) const {
    for (int i = 0; i <= sp; i++) {
        os << setw(8) << stack[i] << '\n';
    }
}

void vm::print_memory(ostream& os) const {
    for (int i = 0; i < mc; i++) {
        os << setw(8) << data_memory[i] << '\n';
    }
}