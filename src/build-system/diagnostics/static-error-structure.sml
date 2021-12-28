structure StaticErrorStructure =
struct
(* models' LSP's diagnostic information *)
    datatype diagnosticseverity = DiagnosticError | DiagnosticWarning | DiagnosticInformation | DiagnosticHint
    datatype staticerror = 
        StaticError of UTF8String.t  (* the location of the error *)
                    * diagnosticseverity
                    (* * (int * string) option code and code description uri *)
                    * string (* message *)

    type errlist = staticerror list

    datatype 'a witherrsoption = Success of 'a | DErrors of errlist | NotAvailable

    fun fmap (f : 'a -> 'b ) (x : 'a witherrsoption) : 'b witherrsoption = 
        case x of
            Success y => Success (f y)
            | DErrors l => DErrors l
            | NotAvailable  => NotAvailable

    exception NotSuccess
    fun isSuccess  (x : 'a witherrsoption) : bool =
        case x of
            Success y => true
            | _ => false
    
    fun isNotSuccess  (x : 'a witherrsoption) : bool =
        not (isSuccess x)

    fun valOf  (x : 'a witherrsoption) : 'a =
        case x of
            Success y => y
            | DErrors l => raise NotSuccess
            | NotAvailable => raise NotSuccess
        
    fun next (x : 'a witherrsoption) (f : 'a -> 'b witherrsoption) : 'b witherrsoption = 
        case x of
            Success y => (f y)
            | DErrors l => DErrors l
            | NotAvailable  => NotAvailable
    fun >>= (x, f) = next x f
end