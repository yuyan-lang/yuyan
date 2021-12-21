structure LLVMAst = struct


type llvmlocation = int (* this corresponds directly to cpsvar *)
datatype llvmvalue = LLVMLocalVar of int
                   | LLVMStringVar of int * UTF8String.t (* for calculating length *)
                   | LLVMFunctionVar of int * int (* argument count *)
                   | LLVMIntConst of int
datatype llvmarraytype = 
        LLVMArrayTypeFunctionClosure (* for storing continuation cosures generated during compilation *)
        | LLVMArrayTypeFold
        | LLVMArrayTypeProd
        | LLVMArrayTypeSum
        | LLVMArrayTypeUnit

        

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
type llvmsignature = llvmdeclaration list

end