structure LLVMAst = struct


type llvmlocation = int (* this corresponds directly to cpsvar *)
datatype llvmstatement = 
    LLVMStoreUnit of llvmlocation
    | LLVMStoreArray of llvmlocation * int list
    (* this is the same as store function array except the first argument 
    is interpreted as the name of the function *)
    | LLVMStoreFunctionClosure of llvmlocation * int list
    | LLVMArrayAccess of llvmlocation (* result *) 
                    * int  (* POINTER to array *)
                    * int  (* index *)
    | LLVMConditionalJump of int (* VARIABLE NAME that stores the index *) 
            * llvmstatement list list (* one block for each index *)
    | LLVMCall of int (* function name *)
            * int list (* function arguments *)
    | LLVMReturn

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