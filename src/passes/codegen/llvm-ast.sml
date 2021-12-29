structure LLVMAst = struct


type llvmlocation = int (* this corresponds directly to cpsvar *)
datatype llvmvalue = LLVMLocalVar of int (* appear as  %v(i) *)
                   | LLVMStringName of int * UTF8String.t (* for calculating length *) (* appear as @s(i) *)
                   | LLVMFunctionName of int * int (* argument count *) (* appear as @f(i) *)
                   | LLVMIntConst of int (* directly stored as int *)
datatype llvmarraytype = 
        LLVMArrayTypeFunctionClosure (* for storing continuation cosures generated during compilation *)
        | LLVMArrayTypeFold
        | LLVMArrayTypeProd
        | LLVMArrayTypeSum
        | LLVMArrayTypeUnit
        | LLVMArrayTypeString (* for storing a string *)

        

datatype llvmstatement = 
    LLVMStoreUnit of llvmlocation
    | LLVMStoreArray of llvmarraytype * llvmlocation * llvmvalue list
    (* this is the same as store function array except the first argument 
    is interpreted as the name of the function *)
    | LLVMArrayAccess of llvmlocation (* result *) 
                    * int  (* POINTER to array *)
                    * int  (* index *)
(* conditional jump and call should not be followed by any other statement 
TODO: Maybe we want to make that syntactically explicit *)
    | LLVMConditionalJump of int (* VARIABLE NAME that stores the index *) 
            * llvmstatement list list (* one block for each index *)
    | LLVMCall of int (* function name *)
            * int list (* function arguments *)
    | LLVMFfiCCall of  llvmlocation (* result of the function call *)
                * UTF8String.t (* function name *)
            * llvmvalue list (* function arguments *)
    | LLVMReturn of int (* Variable name that stores the result *)

datatype llvmdeclaration  = LLVMFunction of int (* name of the function *) 
                                          * int list (* list of arguments *)
                                          * llvmstatement list
                          | LLVMStringConstant of int (* global name *) 
                                    *  UTF8String.t
                          | LLVMBoolConstant of int (* global name *) 
                                    *  bool
                          | LLVMIntConstant of int (* global name *) 
                                    *  int
                          | LLVMRealConstant of int (* global name *) 
                                    *  real
                          | LLVMFfiFunction of UTF8String.t (* global name *) 
                                    *  int (* number of arguments *)
type llvmsignature = int * llvmdeclaration list (* entry func name plus a list of llvm declarations *)

end