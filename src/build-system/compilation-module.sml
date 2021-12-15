structure CompilationModule =
struct
  open CompilationTokens
    (* file path is the key *)
    type yymodule = 
             (
                 (* the compiled type checking signature *)
               TypeCheckingAST.Signature 
              (* list of parse tokens, for LSP *)
              * token list 
             ) StrDict.dict
    
end