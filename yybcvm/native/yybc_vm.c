#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "common_include.h"
#include "yystdlib_include.h"

// Enum for the opcodes
typedef enum {
    LoadParam = 1,
    LoadLocal,
    StoreLocal,
    ReadTuple,
    WriteTuple,
    MakeTuple,
    CallFuncPtr,
    ExternalCall,
    IntConst,
    StringConst,
    BranchIfFalse,
    Branch,
    Label,
    BeginFunc,
    EndFunc,
    Return,
    PopOpStack,
    BoolConst,
    UnitConst,
    DecimalConst,
    FuncRef,
    FileRef,
    UpdateFileRef
} Opcode;

// VM memory regions
uint8_t *byte_code_data;
uint8_t *program_section; // For instructions
char** string_table;   // For strings (data section)
yyvalue* op;           // Operator stack pointer
yyvalue* op_start;           // Operator stack pointer
uint8_t* pc;           // Program counter (text section)
uint8_t **labels_table;
uint8_t **funcs_table;
yyvalue *files_table;

void print_current_disassemble_single(uint8_t *ptr);

// Function to swap bytes for 32-bit values
uint32_t swap_uint32(uint32_t val) {
    return ntohl(val); // Convert from big-endian to host-endian
}

// Function to swap bytes for 64-bit values
uint64_t swap_uint64(uint64_t val) {
    return (((uint64_t)ntohl(val & 0xFFFFFFFF)) << 32) | ntohl(val >> 32);
}

// Function to load the bytecode and initialize the VM memory
void load_bytecode(const char* filename) {
    FILE* f = fopen(filename, "rb");
    if (!f) {
        perror("Failed to open file");
        exit(1);
    }

    // Read header (64 bytes)
    uint64_t num_external_calls, num_files, num_funcs, num_strings, string_offset;
    fread(&num_external_calls, sizeof(uint64_t), 1, f);
    fread(&num_files, sizeof(uint64_t), 1, f);
    fread(&num_funcs, sizeof(uint64_t), 1, f);
    fread(&num_strings, sizeof(uint64_t), 1, f);
    fread(&string_offset, sizeof(uint64_t), 1, f);

    num_external_calls = swap_uint64(num_external_calls);
    num_files = swap_uint64(num_files);
    num_funcs = swap_uint64(num_funcs);
    num_strings = swap_uint64(num_strings);
    string_offset = swap_uint64(string_offset);


    // Allocate memory for strings and instruction text section
    string_table = (char**)malloc(num_strings * sizeof(char*));
    labels_table = (uint8_t**)malloc(num_strings * sizeof(uint8_t*));
    funcs_table = (uint8_t**)malloc(num_strings * sizeof(uint8_t*));
    files_table = (yyvalue*)malloc(num_strings * sizeof(yyvalue));
    // Allocate memory for the entire bytecode section
    // Size includes the part after the header up to the end of the file
    fseek(f, 0, SEEK_END);
    long fileSize = ftell(f);
    fseek(f, 64, SEEK_SET); // Move to the bytecode data start

    // Allocate bytecode data
    long byte_code_size = fileSize - 64;
    byte_code_data = (uint8_t*)malloc(byte_code_size);
    fread(byte_code_data, sizeof(uint8_t), byte_code_size, f);

    fclose(f);

    // 
    uint8_t* string_data = byte_code_data + string_offset - 64; // Start of string data

    for (uint64_t i = 0; i < num_strings; i++) {
            // Check if we are at the end of byte_code_data
        if (string_data >= byte_code_data + fileSize) {
            fprintf(stderr, "Warning: Reached end of bytecode data before reading all strings.\n");
            exit(1);
            break; // Stop reading if we reach the end
        }
        // Point to the actual string data in byte_code_data
        string_table[i] = (char*)string_data; // No allocation needed, just point to it

        // Find the length of the string by traversing until the null terminator
        while (*string_data != '\0') {
            string_data++; // Move to the end of the current string
        }

        // Move past the null terminator to the start of the next string
        string_data++;
    }


    // Iterate through byte_code_data to populate labels and funcs tables
    pc = byte_code_data; // Program counter
    while (pc < byte_code_data + string_offset-64) {
        Opcode opcode = *pc; // Get current opcode

        switch (opcode) {
            case LoadParam:
            case LoadLocal:
            case StoreLocal:
            case MakeTuple:
            case BranchIfFalse:
            case Branch:
            case EndFunc:
            case FuncRef:
            case FileRef:
            case UpdateFileRef:
            case StringConst:
                // For these opcodes, simply move the program counter forward
                pc += 1 + sizeof(uint32_t); // Move past opcode + 4 bytes for the index
                break;

            case BoolConst:
                pc += 1 + 1; // Move past opcode + 1 byte for the value
                break;

            case CallFuncPtr:
            case ExternalCall:
            case IntConst:
            case DecimalConst:
                // IntConst is 1 byte opcode + 8 bytes for the int value
                pc += 1 + sizeof(uint64_t); // Move past opcode + 8 bytes for the value
                break;


            case Label:
            case BeginFunc: {

                // For these opcodes, read the index and update the tables
                uint32_t index;
                memcpy(&index, pc+1, sizeof(uint32_t)); // Read the index
                index = swap_uint32(index); // Convert from big-endian to host-endian

                // Populate labels or funcs table as appropriate
                if (opcode == Label) {
                    labels_table[index] = pc;
                } else {
                    // printf("funcs_table[%d] = %p\n", index, pc);
                    // print_current_disassemble_single(pc);
                    funcs_table[index] = pc;
                }
                
                pc += 1; // Move past the opcode
                pc += sizeof(uint32_t); // Move past the index
                break;
            }


            case ReadTuple:
            case WriteTuple:
            case Return:
            case UnitConst:
            case PopOpStack:
                // No additional data, just move the program counter forward
                pc += 1; // Move past the opcode
                break;

            default:
                fprintf(stderr, "Unknown opcode encountered: %d\n", opcode);
                exit(1); // Handle unknown opcode gracefully
        }
    }

    op_start = malloc(1024 * sizeof(yyvalue));
    op = op_start;
    pc = byte_code_data;
}

yyvalue yyCallCC() {
    return stackptr_to_yyvalue(stack_ptr);
}

yyvalue yyCallCCRet(yyvalue stack_ptr_address, yyvalue ret_val) {
    *op = ret_val;
    op++;
    yyvalue *saved_stack_ptr = yyvalue_to_stackptr(stack_ptr_address);
    stack_ptr = yyvalue_to_staticptr(saved_stack_ptr[-2]);
    pc =  (uint8_t *)yyvalue_to_staticptr(saved_stack_ptr[-1]);
    return unit_to_yyvalue();
}

yyvalue do_yy_external_call(char * callName, uint32_t nargs, yyvalue args[]){
// "yyRunningOnWindows", "yyNewRef", "yyReadRef", "yyWriteRef",
// "yyIntAdd", "yyIntMult", "yyIntDiv", "yyIntSub", "yyIntEqTest",
// "yyIntGtTest", "yyIntToString", "yyStringEq", "yyStringToInt", "yyGetCommandLineProgramName",
// "yyGetCommandLineArgs", "yyPrintln", "yyPrintStr", "yyPrintlnStdErr",
// "yyNewRefArrayGeneric", "yyWriteRefArray", "yyReadRefArray", "yyCodePointsConcat",
// "yyGetCurrentWorkingDirectory", "yyGetCodePoints", "yyGetCommandLineArgs", "yyGetCurrentLocalDateTimeFmt",
// "yyPrintlnStdErr", "获取当前异常处理器", "设置当前异常处理器", "yyExceptCallCC", "yyGlobalExcept",
// "yyIsSubstring", "yyPrintln", "yyPathExists", "yyGetFileModifiedTime", "yyReadFileSync",
// "yy_豫言字符串获取字节数组", "yyStringByteArrayGetLength", "yy_豫言字符串获取字节序数当前字符", "yy_豫言字符串匹配",
// "yyIsPathRegularFile", "yyIsPathDirectory", "yyWriteFileSync", "yyProcessExit", "yyCallCC", "yyCallCCRet",
// 'yyDeleteFileSync', 'yyPrintGeneric', 'yyDoubleToString', 'yyRunningOnMacOS', 'yy_豫言字符串获取JSON字符串', 
// 'yyGetRandomInt', 'yyDoubleAdd', 'yyIntToDouble', 'yyDoubleToInt', 'yy_豫言子字符串从字节序数开始', 'yyDoubleSub', 
// 'yyListDirectorySync', 'yyGetRandomDouble', 'yy_豫言不安全转换', 'yyStringByteLength', 'yyNewRefArray', 
// 'yyGetCurrentLocalDateTimeStr', 'yyRunProcessSyncPipeOutput', 'yyDoubleMult', 'yyStringToDouble', 'yyDoubleDiv', 
// 'yyRunProcessGetOutputSync', 'yy_豫言字符转整数', 'yyRunningOnLinux', 'yyRunProcessSync', 'yyCurrentNanosecondTime'
    if (strcmp(callName, "yyRunningOnWindows") == 0) {
        assert(nargs == 0);
        return yyRunningOnWindows();
    } else if (strcmp(callName, "yyNewRef") == 0) {
        assert(nargs == 1);
        return yyNewRef(args[0]);
    } else if (strcmp(callName, "yyReadRef") == 0) {
        assert(nargs == 1);
        return yyReadRef(args[0]);
    } else if (strcmp(callName, "yyWriteRef") == 0) {
        assert(nargs == 2);
        return yyWriteRef(args[0], args[1]);
    } else if (strcmp(callName, "yyIntAdd") == 0) {
        assert(nargs == 2);
        return yyIntAdd(args[0], args[1]);
    } else if (strcmp(callName, "yyIntMult") == 0) {
        assert(nargs == 2);
        return yyIntMult(args[0], args[1]);
    } else if (strcmp(callName, "yyIntDiv") == 0) {
        assert(nargs == 2);
        return yyIntDiv(args[0], args[1]);
    } else if (strcmp(callName, "yyIntSub") == 0) {
        assert(nargs == 2);
        return yyIntSub(args[0], args[1]);
    } else if (strcmp(callName, "yyIntEqTest") == 0) {
        assert(nargs == 2);
        return yyIntEqTest(args[0], args[1]);
    } else if (strcmp(callName, "yyIntGtTest") == 0) {
        assert(nargs == 2);
        return yyIntGtTest(args[0], args[1]);
    } else if (strcmp(callName, "yyIntToString") == 0) {
        assert(nargs == 1);
        return yyIntToString(args[0]);
    } else if (strcmp(callName, "yyStringEq") == 0) {
        assert(nargs == 2);
        return yyStringEq(args[0], args[1]);
    } else if (strcmp(callName, "yyStringToInt") == 0) {
        assert(nargs == 1);
        return yyStringToInt(args[0]);
    } else if (strcmp(callName, "yyGetCommandLineProgramName") == 0) {
        assert(nargs == 0);
        return yyGetCommandLineProgramName();
    } else if (strcmp(callName, "yyGetCommandLineArgs") == 0) {
        assert(nargs == 0);
        return yyGetCommandLineArgs();
    } else if (strcmp(callName, "yyPrintln") == 0) {
        assert(nargs == 1);
        return yyPrintln(args[0]);
    } else if (strcmp(callName, "yyPrintStr") == 0) {
        assert(nargs == 1);
        return yyPrintStr(args[0]);
    } else if (strcmp(callName, "yyPrintlnStdErr") == 0) {
        assert(nargs == 1);
        return yyPrintlnStdErr(args[0]);
    } else if (strcmp(callName, "yyNewRefArrayGeneric") == 0) {
        assert(nargs == 1);
        return yyNewRefArrayGeneric(args[0]);
    } else if (strcmp(callName, "yyWriteRefArray") == 0) {
        assert(nargs == 3);
        return yyWriteRefArray(args[0], args[1], args[2]);
    } else if (strcmp(callName, "yyReadRefArray") == 0) {
        assert(nargs == 2);
        return yyReadRefArray(args[0], args[1]);
    } else if (strcmp(callName, "yyCodePointsConcat") == 0) {
        assert(nargs == 1);
        return yyCodePointsConcat(args[0]);
    } else if (strcmp(callName, "yyGetCurrentWorkingDirectory") == 0) {
        assert(nargs == 0);
        return yyGetCurrentWorkingDirectory();
    } else if (strcmp(callName, "yyGetCodePoints") == 0) {
        assert(nargs == 1);
        return yyGetCodePoints(args[0]);
    } else if (strcmp(callName, "yyGetCurrentLocalDateTimeFmt") == 0) {
        assert(nargs == 1);
        return yyGetCurrentLocalDateTimeFmt(args[0]);
    } else if (strcmp(callName, "yyGetCommandLineArgs") == 0) {
        assert(nargs == 0);
        return yyGetCommandLineArgs();
    } else if (strcmp(callName, "yyPrintlnStdErr") == 0) {
        assert(nargs == 1);
        return yyPrintlnStdErr(args[0]);
    } else if (strcmp(callName, "获取当前异常处理器") == 0) {
        assert(nargs == 0);
        return 获取当前异常处理器();
    } else if (strcmp(callName, "设置当前异常处理器") == 0) {
        assert(nargs == 1);
        return 设置当前异常处理器(args[0]);
    } else if (strcmp(callName, "yyIsSubstring") == 0) {
        assert(nargs == 2);
        return yyIsSubstring(args[0], args[1]);
    } else if (strcmp(callName, "yyPrintln") == 0) {
        assert(nargs == 1);
        return yyPrintln(args[0]);
    } else if (strcmp(callName, "yyPathExists") == 0) {
        assert(nargs == 1);
        return yyPathExists(args[0]);
    } else if (strcmp(callName, "yyGetFileModifiedTime") == 0) {
        assert(nargs == 1);
        return yyGetFileModifiedTime(args[0]);
    } else if (strcmp(callName, "yyReadFileSync") == 0) {
        assert(nargs == 1);
        return yyReadFileSync(args[0]);
    } else if (strcmp(callName, "yy_豫言字符串获取字节数组") == 0) {
        assert(nargs == 1);
        return yy_豫言字符串获取字节数组(args[0]);
    } else if (strcmp(callName, "yyStringByteArrayGetLength") == 0) {
        assert(nargs == 1);
        return yyStringByteArrayGetLength(args[0]);
    } else if (strcmp(callName, "yy_豫言字符串获取字节序数当前字符") == 0) {
        assert(nargs == 2);
        return yy_豫言字符串获取字节序数当前字符(args[0], args[1]);
    } else if (strcmp(callName, "yy_豫言字符串匹配") == 0) {
        assert(nargs == 3);
        return yy_豫言字符串匹配(args[0], args[1], args[2]);
    } else if (strcmp(callName, "yyIsPathRegularFile") == 0) {
        assert(nargs == 1);
        return yyIsPathRegularFile(args[0]);
    } else if (strcmp(callName, "yyIsPathDirectory") == 0) {
        assert(nargs == 1);
        return yyIsPathDirectory(args[0]);
    } else if (strcmp(callName, "yyWriteFileSync") == 0) {
        assert(nargs == 2);
        return yyWriteFileSync(args[0], args[1]);
    } else if (strcmp(callName, "yyProcessExit") == 0) {
        assert(nargs == 1);
        return yyProcessExit(args[0]);
    } else if (strcmp(callName, "yyCallCC") == 0) {
        assert(nargs == 0);
        return yyCallCC();
    } else if (strcmp(callName, "yyCallCCRet") == 0) {
        assert(nargs == 2);
        return yyCallCCRet(args[0], args[1]);
    } else if (strcmp(callName, "yyDeleteFileSync") == 0) {
        assert(nargs == 1);
        return yyDeleteFileSync(args[0]);
    } else if (strcmp(callName, "yyPrintGeneric") == 0) {
        assert(nargs == 2);
        return yyPrintGeneric(args[0], args[1]);
    } else if (strcmp(callName, "yyDoubleToString") == 0) {
        assert(nargs == 1);
        return yyDoubleToString(args[0]);
    } else if (strcmp(callName, "yyRunningOnMacOS") == 0) {
        assert(nargs == 0);
        return yyRunningOnMacOS();
    } else if (strcmp(callName, "yy_豫言字符串获取JSON字符串") == 0) {
        assert(nargs == 2);
        return yy_豫言字符串获取JSON字符串(args[0], args[1]);
    } else if (strcmp(callName, "yyGetRandomInt") == 0) {
        assert(nargs == 1);
        return yyGetRandomInt(args[0]);
    } else if (strcmp(callName, "yyDoubleAdd") == 0) {
        assert(nargs == 2);
        return yyDoubleAdd(args[0], args[1]);
    } else if (strcmp(callName, "yyIntToDouble") == 0) {
        assert(nargs == 1);
        return yyIntToDouble(args[0]);
    } else if (strcmp(callName, "yyDoubleToInt") == 0) {
        assert(nargs == 1);
        return yyDoubleToInt(args[0]);
    } else if (strcmp(callName, "yy_豫言子字符串从字节序数开始") == 0) {
        assert(nargs == 2);
        return yy_豫言子字符串从字节序数开始(args[0], args[1]);
    } else if (strcmp(callName, "yyDoubleSub") == 0) {
        assert(nargs == 2);
        return yyDoubleSub(args[0], args[1]);
    } else if (strcmp(callName, "yyListDirectorySync") == 0) {
        assert(nargs == 1);
        return yyListDirectorySync(args[0]);
    } else if (strcmp(callName, "yyGetRandomDouble") == 0) {
        assert(nargs == 2);
        return yyGetRandomDouble(args[0], args[1]);
    } else if (strcmp(callName, "yy_豫言不安全转换") == 0) {
        assert(nargs == 1);
        return yy_豫言不安全转换(args[0]);
    } else if (strcmp(callName, "yyStringByteLength") == 0) {
        assert(nargs == 1);
        return yyStringByteLength(args[0]);
    } else if (strcmp(callName, "yyNewRefArray") == 0) {
        assert(nargs == 2);
        return yyNewRefArray(args[0], args[1]);
    } else if (strcmp(callName, "yyGetCurrentLocalDateTimeStr") == 0) {
        assert(nargs == 1);
        return yyGetCurrentLocalDateTimeStr(args[0]);
    } else if (strcmp(callName, "yyRunProcessSyncPipeOutput") == 0) {
        assert(nargs == 2);
        return yyRunProcessSyncPipeOutput(args[0], args[1]);
    } else if (strcmp(callName, "yyDoubleMult") == 0) {
        assert(nargs == 2);
        return yyDoubleMult(args[0], args[1]);
    } else if (strcmp(callName, "yyStringToDouble") == 0) {
        assert(nargs == 1);
        return yyStringToDouble(args[0]);
    } else if (strcmp(callName, "yyDoubleDiv") == 0) {
        assert(nargs == 2);
        return yyDoubleDiv(args[0], args[1]);
    } else if (strcmp(callName, "yyRunProcessGetOutputSync") == 0) {
        assert(nargs == 2);
        return yyRunProcessGetOutputSync(args[0], args[1]);
    } else if (strcmp(callName, "yy_豫言字符转整数") == 0) {
        assert(nargs == 2);
        return yy_豫言字符转整数(args[0], args[1]);
    } else if (strcmp(callName, "yyRunningOnLinux") == 0) {
        assert(nargs == 0);
        return yyRunningOnLinux();
    } else if (strcmp(callName, "yyRunProcessSync") == 0) {
        assert(nargs == 2);
        return yyRunProcessSync(args[0], args[1]);
    } else if (strcmp(callName, "yyCurrentNanosecondTime") == 0) {
        assert(nargs == 0);
        return yyCurrentNanosecondTime();
    } else {
        fprintf(stderr, "Unknown external call: %s\n", callName);
        exit(1);
    }
}

// Helper functions to read 32-bit and 64-bit values from text section
uint32_t read_uint32(uint8_t* pc) {
    uint32_t val;
    memcpy(&val, pc, sizeof(uint32_t));
    return swap_uint32(val);
}

uint64_t read_uint64(uint8_t* pc) {
    uint64_t val;
    memcpy(&val, pc, sizeof(uint64_t));
    return swap_uint64(val);
}

void print_current_disassemble_single(uint8_t *ptr) {
    uint8_t opcode = *ptr++; // Fetch the opcode

    switch (opcode) {
        case LoadParam: {
            uint32_t idx = read_uint32(ptr);
            printf("LoadParam %d\n", idx);
            break;
        }
        case LoadLocal: {
            uint32_t idx = read_uint32(ptr);
            printf("LoadLocal %d\n", idx);
            break;
        }
        case StoreLocal: {
            uint32_t idx = read_uint32(ptr);
            printf("StoreLocal %d\n", idx);
            break;
        }
        case ReadTuple: {
            printf("ReadTuple\n");
            break;
        }
        case WriteTuple: {
            printf("WriteTuple\n");
            break;
        }
        case MakeTuple: {
            uint32_t len = read_uint32(ptr);
            printf("MakeTuple %d\n", len);
            break;
        }
        case IntConst: {
            uint64_t val = read_uint64(ptr);
            printf("IntConst %ld\n", val);
            break;
        }
        case StringConst: {
            uint32_t idx = read_uint32(ptr);
            printf("StringConst %d\n", idx);
            break;
        }
        case BoolConst: {
            uint8_t val = *ptr;
            printf("BoolConst %d\n", val);
            break;
        }
        case UnitConst: {
            printf("UnitConst\n");
            break;
        }
        case DecimalConst: {
            uint64_t val = read_uint64(ptr);
            double val_d = *(double*)&val;
            printf("DecimalConst %f\n", val_d);
            break;
        }
        case CallFuncPtr: {
            uint32_t nargs = read_uint32(ptr);
            uint32_t stack_offset = read_uint32(ptr + 4);
            printf("CallFuncPtr %d %d\n", nargs, stack_offset);
            break;
        }
        case Return: {
            printf("Return\n");
            break;
        }
        case FuncRef: {
            uint32_t idx = read_uint32(ptr);
            printf("FuncRef %d\n", idx);
            break;
        }
        case FileRef: {
            uint32_t idx = read_uint32(ptr);
            printf("FileRef %d\n", idx);
            break;
        }
        case UpdateFileRef: {
            uint32_t idx = read_uint32(ptr);
            printf("UpdateFileRef %d\n", idx);
            break;
        }
        case EndFunc: {
            printf("EndFunc\n");
            exit(1);
        }
        case Label: {
            uint32_t idx = read_uint32(ptr);
            printf("Label %d\n", idx);
            break;
        }
        case BeginFunc: {
            uint32_t idx = read_uint32(ptr);
            printf("BeginFunc %d\n", idx);
            break;
        }
        case ExternalCall: {
            uint32_t name_idx = read_uint32(ptr);
            uint32_t nargs = read_uint32(ptr+4);
            printf("ExternalCall %d %d\n", name_idx, nargs);
            break;
        }
        case BranchIfFalse: {
            uint32_t idx = read_uint32(ptr);
            printf("BranchIfFalse %d\n", idx);
            break;
        }
        case Branch: {
            uint32_t idx = read_uint32(ptr);
            printf("Branch %d\n", idx);
            break;
        }



        default:
            printf("[Disassemble] Unknown opcode %u\n", opcode);
            exit(1); // Stop on error
            break;
    }

}

uint64_t inst_count = 0;

// Function to execute the bytecode
void execute_vm() {
    while (true) {
        printf("%ld: ", ++inst_count);
        print_current_disassemble_single(pc);
        uint8_t opcode = *pc++; // Fetch the opcode

        switch (opcode) {
            case LoadParam: {
                uint32_t idx = read_uint32(pc);
                // printf("LoadParam %d %p\n", -3-idx, stack_ptr);
                int32_t stack_idx = -3-idx;
                yyvalue val = stack_ptr[stack_idx];
                *op = val;
                op++;
                pc+=4;
                break;
            }
            case LoadLocal: {
                uint32_t idx = read_uint32(pc);
                *op = stack_ptr[idx];
                op++;
                pc+=4;
                break;
            }
            case StoreLocal: {
                uint32_t idx = read_uint32(pc);
                stack_ptr[idx] = *(op-1);
                op--;
                pc+=4;
                break;
            }
            case ReadTuple: {
                yyvalue *tuple = yyvalue_to_tuple(*(op - 2));
                uint64_t idx = yyvalue_to_int(*(op-1));
                yyvalue val = tuple[idx];
                op -= 2;
                *op = val;
                op++;
                break;
            }
            case WriteTuple: {
                yyvalue *tuple = yyvalue_to_tuple(*(op - 3));
                uint64_t idx = yyvalue_to_int(*(op-2));
                tuple[idx] = *(op-1);
                op -= 3;
                break;
            }
            case MakeTuple: {
                uint32_t len = read_uint32(pc);
                pc += 4;
                yyvalue elems[len];
                for (int i = 0; i < len; i++) {
                    elems[i] = *(op - len + i);
                }
                yyvalue tuple = tuple_to_yyvalue(len, elems);
                op -= len;
                *op = tuple;
                op++;
                break;
            }
            case IntConst: {
                uint64_t val = read_uint64(pc);
                // bit cast into signed 64 bit integer
                int64_t v = (int64_t)val;
                yyvalue v_int = int_to_yyvalue(val);
                *op = v_int;
                op++;
                pc+=8;
                break;
            }
            case StringConst: {
                uint32_t idx = read_uint32(pc);
                pc += 4;
                char * str = string_table[idx];
                yyvalue v_str = static_string_to_yyvalue(str);
                break;
            }
            case BoolConst: {
                uint8_t val = *pc;
                pc++;
                yyvalue v_bool = bool_to_yyvalue(val);
                *op = v_bool;
                op++;
                break;
            }
            case UnitConst: {
                yyvalue v_unit = unit_to_yyvalue();
                *op = v_unit;
                op++;
                break;
            }
            case DecimalConst: {
                uint64_t val = read_uint64(pc);
                pc += 8;
                double val_d = *(double*)&val;
                yyvalue v_decimal = double_to_yyvalue(val_d);
                *op = v_decimal;
                op++;
                break;
            }
            case CallFuncPtr: {
                uint32_t nargs = read_uint32(pc);
                uint32_t stack_offset = read_uint32(pc + 4);
                pc += 8;
                yyvalue func_ptr = *(op - 1);
                yyvalue args[nargs];
                for (int i = 0; i < nargs; i++) {
                    args[i] = *(op - 2 - i);
                }
                op -= nargs + 1;

                // copy the arguments to the top of the stack
                // stack_ptr + stack_offset , then arg_n, ..., arg_0
                for (int i = 0; i < nargs; i++) {
                    stack_ptr[stack_offset + i] = args[nargs - i - 1];
                }
                // push the previous stack_ptr
                stack_ptr[stack_offset + nargs] = stackptr_to_yyvalue(stack_ptr);
                // push the previous pc
                stack_ptr[stack_offset + nargs + 1] = staticptr_to_yyvalue((yyvalue *)pc);
                // set the new stack_ptr
                stack_ptr = stack_ptr + stack_offset + nargs + 2;
                uint64_t func_idx = yyvalue_to_int(func_ptr);
                printf("Calling %lu %s\n", func_idx, string_table[func_idx]);
                uint8_t *new_func = funcs_table[func_idx];
                pc = new_func;
                break;
            }
            case Return: {
                yyvalue ret = *(op - 1);
                op--;
                pc = (uint8_t *)yyvalue_to_staticptr(stack_ptr[-1]);
                stack_ptr = yyvalue_to_stackptr(stack_ptr[-2]);
                *op = ret;
                op++;
                break;
            }
            case FuncRef: {
                uint32_t idx = read_uint32(pc);
                pc += 4;
                yyvalue func_ref = int_to_yyvalue(idx);
                *op = func_ref;
                op++;
                break;
            }
            case FileRef: {
                uint32_t idx = read_uint32(pc);
                pc += 4;
                yyvalue file_ref = files_table[idx];
                *op = file_ref;
                op++;
                break;
            }
            case UpdateFileRef: {
                uint32_t idx = read_uint32(pc);
                pc += 4;
                yyvalue file_ref = *(op - 1);
                files_table[idx] = file_ref;
                op--;
                break;
            }
            case EndFunc: {
                fprintf(stderr, "ERROR: End of function reached\n");
                exit(1);
            }
            case Label: {
                uint32_t idx = read_uint32(pc);
                pc += 4;
                break;
            }
            case BeginFunc: {
                // just start execute this function as calls will always branch here
                pc += 4;
                break;
            }
            case ExternalCall: {
                uint32_t name_idx = read_uint32(pc);
                pc += 4;
                uint32_t nargs = read_uint32(pc);
                pc += 4;
                yyvalue args[nargs];
                for (int i = 0; i < nargs; i++) {
                    args[i] = *(op - 1 - i);
                }
                op -= nargs;
                char * externalCallName = string_table[name_idx];
                yyvalue ret = do_yy_external_call(externalCallName, nargs, args);
                *op = ret;
                op++;
                break;
            }
            case BranchIfFalse: {
                yyvalue val = *(op - 1);
                op--;
                uint32_t target_label = read_uint32(pc);
                pc += 4;
                bool cond = yyvalue_to_bool(val);
                if (!cond) {
                    pc = labels_table[target_label];
                }
                break;
            }
            case Branch: {
                uint32_t target_label = read_uint32(pc);
                pc += 4;
                pc = labels_table[target_label];
                break;
            }



            default:
                printf("Unknown opcode %u\n", opcode);
                exit(1); // Stop on error
                break;
        }
    }
}

int64_t yy_runtime_start() {
    load_bytecode(main_bytecode_filename);
    initialize_runtime_stack();

    /**
     * Stack will be arranged like this
     * 
     * Yuyan Call Routine
     * 
     * 
     * For function call, the following values will be pushed to the stack
     * arg n
     * arg n-1
     * ...
     * arg 2
     * arg 1
     * arg 0 (! arguments starts from 0)
     * previous stack_ptr
     * previous pc
     * (SP)
     * local 0
     * local 1
     * ...
     *
     * 
     * 
     * 
     * 
     * 
    */
    stack_ptr[0] = unit_to_yyvalue();
    stack_ptr[1] = unit_to_yyvalue(); // a program should not return from entry main, but explicitly call Exit
    // a return from entry main will cause segmentation fault
    stack_ptr += 2;

    execute_vm();
    return 0;
}
