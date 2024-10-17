
from __future__ import annotations

from typing import *
import struct
from dataclasses import dataclass


# load the ith parameter (starting from 0th) to the top of the op stack
# ... -> ..., param_i
@dataclass
class LoadParam:
    idx: int

# load the ith local variable (starting from 0th) to the top of the op stack
# ... -> ..., local_i
@dataclass
class LoadLocal:
    idx: int

# store the top of the op stack to the ith local variable (starting from 0th)
# ..., val -> ...
@dataclass
class StoreLocal:
    idx: int


# read the ith field 
# ..., tuple_ref, idx -> ..., tuple_ref[idx]
@dataclass
class ReadTuple:
    pass

# write the ith field
# ..., tuple_ref, idx, val -> ...
@dataclass
class WriteTuple:
    pass

# create a new tuple with the given length, with elements from the stack
# ..., val_1, val_2, ..., val_n -> ..., tuple_ref
@dataclass
class MakeTuple:
    length: int

# call a function with the given number of arguments
# ..., arg_n, ..., arg_1, func -> ..., result
@dataclass
class CallFuncPtr:
    nargs: int

# external call
# ..., arg_n, ..., arg_1 -> ..., result
@dataclass
class ExternalCall:
    name: int # index into the string table
    nargs: int

# ..., -> ..., val
@dataclass
class IntConst:
    val: int

@dataclass
class BoolConst:
    val: bool

@dataclass
class UnitConst:
    pass

@dataclass
class DecimalConst:
    integral: str
    fractional: str

# ..., -> ..., val
@dataclass
class StringConst:
    val: int # index into the string table

# ..., bool_val -> ...
@dataclass
class BranchIfFalse:
    target: int # index into the string table (label name)

# ..., -> ...
@dataclass
class Branch:
    target: int # index into the string table (label name)

# ..., return_val -> ...
@dataclass
class Return:
    pass

# ..., discard_val -> ...
@dataclass
class PopOpStack:
    pass

# only as a place holder in code
@dataclass
class Label:
    int: int # index into the string

@dataclass
class BeginFunc:
    name: int # index into the string table

@dataclass
class EndFunc:
    name: int # index into the string table

# ..., -> ..., val
@dataclass
class FuncRef:
    name: int # index into the strings table

# ... -> ..., val
@dataclass
class FileRef:
    name: int # index into the strings table

# ..., val -> ...
@dataclass
class UpdateFileRef:
    name: int # index into the strings table



Instruction = ( LoadParam | LoadLocal | StoreLocal | ReadTuple | WriteTuple | 
            MakeTuple | CallFuncPtr | ExternalCall | IntConst | StringConst | 
            BranchIfFalse | Branch | Label | BeginFunc | EndFunc | Return | PopOpStack
            | BoolConst | UnitConst | DecimalConst | FuncRef | FileRef | UpdateFileRef
            )

def inst_to_text(inst: Instruction) -> str:
    match inst:
        case LoadParam(idx):
            return f"LoadParam {idx}"
        case LoadLocal(idx):
            return f"LoadLocal {idx}"
        case StoreLocal(idx):
            return f"StoreLocal {idx}"
        case ReadTuple():
            return "ReadTuple"
        case WriteTuple():
            return "WriteTuple"
        case MakeTuple(length):
            return f"MakeTuple {length}"
        case CallFuncPtr(nargs):
            return f"CallFuncPtr {nargs}"
        case ExternalCall(name, nargs):
            return f"ExternalCall {name} {nargs}"
        case IntConst(val):
            return f"IntConst {val}"
        case StringConst(val):
            return f"StringConst {val}"
        case BranchIfFalse(target):
            return f"BranchIfFalse {target}"
        case Branch(target):
            return f"Branch {target}"
        case Label(val):
            return f"Label {val}"
        case BeginFunc(name):
            return f"BeginFunc {name}"
        case EndFunc(name):
            return f"EndFunc {name}"
        case Return():
            return "Return"
        case PopOpStack():
            return "PopOpStack"
        case BoolConst(val):
            return f"BoolConst {1 if val else 0}"
        case UnitConst():
            return "UnitConst"
        case DecimalConst(integral, fractional):
            return f"DecimalConst {integral}.{fractional}"
        case FuncRef(name):
            return f"FuncRef {name}"
        case FileRef(name):
            return f"FileRef {name}"
        case UpdateFileRef(name):
            return f"UpdateFileRef {name}"
        case _:
            raise ValueError(f"Unknown instruction {inst}")
        
def write_instr_to_bytes(inst: Instruction) -> bytearray:
    match inst:
        case LoadParam(idx):
            # 0x01 opcode for LoadParam followed by 4-byte index
            return bytearray([0x01]) + struct.pack(">I", idx)
        case LoadLocal(idx):
            # 0x02 opcode for LoadLocal followed by 4-byte index
            return bytearray([0x02]) + struct.pack(">I", idx)
        case StoreLocal(idx):
            # 0x03 opcode for StoreLocal followed by 4-byte index
            return bytearray([0x03]) + struct.pack(">I", idx)
        case ReadTuple():
            # 0x04 opcode for ReadTuple (no additional data)
            return bytearray([0x04])
        case WriteTuple():
            # 0x05 opcode for WriteTuple (no additional data)
            return bytearray([0x05])
        case MakeTuple(length):
            # 0x06 opcode for MakeTuple followed by 4-byte length
            return bytearray([0x06]) + struct.pack(">I", length)
        case CallFuncPtr(nargs):
            # 0x07 opcode for CallFuncPtr followed by 4-byte number of args
            return bytearray([0x07]) + struct.pack(">I", nargs)
        case ExternalCall(name, nargs):
            # 0x08 opcode for ExternalCall followed by 4-byte name index and 4-byte nargs
            return bytearray([0x08]) + struct.pack(">I", name) + struct.pack(">I", nargs)
        case IntConst(val):
            # 0x09 opcode for IntConst followed by 8-byte int value
            return bytearray([0x09]) + struct.pack(">Q", val)
        case StringConst(val):
            # 0x0A opcode for StringConst followed by 4-byte string index
            return bytearray([0x0A]) + struct.pack(">I", val)
        case BranchIfFalse(target):
            # 0x0B opcode for BranchIfFalse followed by 4-byte target index
            return bytearray([0x0B]) + struct.pack(">I", target)
        case Branch(target):
            # 0x0C opcode for Branch followed by 4-byte target index
            return bytearray([0x0C]) + struct.pack(">I", target)
        case Label(val):
            # 0x0D opcode for Label followed by 4-byte label index
            return bytearray([0x0D]) + struct.pack(">I", val)
        case BeginFunc(name):
            # 0x0E opcode for BeginFunc followed by 4-byte function name index
            return bytearray([0x0E]) + struct.pack(">I", name)
        case EndFunc(name):
            # 0x0F opcode for EndFunc followed by 4-byte function name index
            return bytearray([0x0F]) + struct.pack(">I", name)
        case Return():
            # 0x10 opcode for Return (no additional data)
            return bytearray([0x10])
        case PopOpStack():
            # 0x11 opcode for PopOpStack (no additional data)
            return bytearray([0x11])
        case BoolConst(val):
            # 0x12 opcode for BoolConst followed by 1-byte boolean value (1 or 0)
            return bytearray([0x12, 0x01 if val else 0x00])
        case UnitConst():
            # 0x13 opcode for UnitConst (no additional data)
            return bytearray([0x13])
        case DecimalConst(integral, fractional):
            # 0x14 opcode for DecimalConst followed by two 64-bit integers (strings converted)
            int_val = int(integral)
            frac_val = int(fractional)
            return bytearray([0x14]) + struct.pack(">Q", int_val) + struct.pack(">Q", frac_val)
        case FuncRef(name):
            # 0x15 opcode for FuncRef followed by 4-byte function name index
            return bytearray([0x15]) + struct.pack(">I", name)
        case FileRef(name):
            # 0x16 opcode for FileRef followed by 4-byte file name index
            return bytearray([0x16]) + struct.pack(">I", name)
        case UpdateFileRef(name):
            # 0x17 opcode for UpdateFileRef followed by 4-byte file name index
            return bytearray([0x17]) + struct.pack(">I", name)
        case _:
            raise ValueError(f"Unknown instruction {inst}")