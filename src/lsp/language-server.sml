
structure LanguageServer = struct 

    datatype server = Server of CompilationManager.compilationmanager

    type t = server

    fun getCM (server : server) = 
        case server of (Server cm)=> cm

end