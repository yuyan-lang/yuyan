structure URIUtil = struct 
    fun uriToPath (uri : string) : string = 
        if String.isPrefix "file://" uri
        then String.extract (uri, String.size "file://", NONE)
        else uri

end
