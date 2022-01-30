structure LLVMAst = struct


datatype llvmlocation = LLVMLocationLocal of int (* this corresponds directly to cpsvar *)
                      | LLVMLocationGlobal of  int
datatype llvmvalue = LLVMLocalVar of int (* appear as  %v(i) *)
                   | LLVMGlobalVar of int (* appear as @v(i) *)
                   | LLVMStringName of int * UTF8String.t (* for calculating length *) (* appear as @s(i) *)
                   | LLVMFunctionName of int * int (* argument count *) (* appear as @f(i) *)
                   | LLVMIntConst of int (* directly stored as int *)
                   | LLVMIntName of int  (* global int const name *)
                   | LLVMRealName of int  (* global real const name *)
datatype llvmarraytype = 
        LLVMArrayTypeFunctionClosure (* for storing continuation cosures generated during compilation *)
        | LLVMArrayTypeFold
        | LLVMArrayTypeProd
        | LLVMArrayTypeSum
        | LLVMArrayTypeUnit
        | LLVMArrayTypeString (* for storing a string *)
        | LLVMArrayTypeInt
        | LLVMArrayTypeReal
        | LLVMArrayTypeDynClsfd

        
datatype llvmprimitiveop = 
        LLVMPOpCmpEqInt of llvmlocation (* result *)
                                * llvmvalue  (* op1 *)
                                * llvmvalue  (* op2 *)
        | LLVMPOpValueToInt of llvmlocation (* result *)
                                * llvmvalue (* op1 *)

datatype llvmstatement = 
    LLVMStoreUnit of llvmlocation
    | LLVMStoreGlobal of int * llvmvalue  (* load global into local, (dst, src) *)
    | LLVMLoadGlobal of int *  int
    | LLVMStoreArray of llvmarraytype * llvmlocation * llvmvalue list
    (* this is the same as store function array except the first argument 
    is interpreted as the name of the function *)
    | LLVMArrayAccess of llvmlocation (* result *) 
                    * llvmlocation  (* POINTER to array *)
                    * int  (* index *)
(* conditional jump and call should not be followed by any other statement 
TODO: Maybe we want to make that syntactically explicit *)
    | LLVMConditionalJump of int (* VARIABLE NAME that stores the index *) 
            * llvmstatement list list (* one block for each index *)
    | LLVMConditionalJumpBinary of llvmlocation (* Variable name that stores the boolean *)
           * llvmstatement list (* true branch *)
           * llvmstatement list (* false branch branch *)
    | LLVMCall of llvmlocation (* function name *)
            * llvmlocation list (* function arguments *)
    | LLVMFfiCCall of  llvmlocation (* result of the function call *)
                * UTF8String.t (* function name *)
            * llvmvalue list (* function arguments *)
    | LLVMReturn of llvmlocation (* Variable name that stores the result *)
    | LLVMComment of string
    | LLVMPrimitiveOp of llvmprimitiveop

datatype llvmdeclaration  = LLVMFunction of int (* name of the function *) 
                                          * int list (* list of arguments *)
                                          * llvmstatement list
                          | LLVMStringConstant of int (* global name *) 
                                    *  UTF8String.t
                          (* | LLVMBoolConstant of int (* global name *) 
                                    *  bool *)
                          | LLVMIntConstant of int (* global name *) 
                                    *  int
                          | LLVMRealConstant of int (* global name *) 
                                    *  real
                          | LLVMFfiFunction of UTF8String.t (* global name *) 
                                    *  int (* number of arguments *)
                          | LLVMGlobalVariableDecl of int (* global name*)
type llvmsignature = int * llvmdeclaration list (* entry func name plus a list of llvm declarations *)

end