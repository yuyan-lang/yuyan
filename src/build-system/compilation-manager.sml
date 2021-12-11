structure CompilationManager = struct

    datatype yymodule = YYModule
    datatype compilationmanager = 
        YYCM of (UTF8String.t * yymodule) list

    type t = compilationmanager

    fun emptyCM () = YYCM []


    fun initWithWorkingDirectory (pwd : string) =  raise Fail ""
        
end
