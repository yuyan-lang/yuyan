structure StaticErrorStructure =
struct
(* models' LSP's diagnostic information *)
    datatype diagnosticseverity = DiagnosticError | DiagnosticWarning | DiagnosticInformation | DiagnosticHint
    datatype staticerror = 
        StaticError of UTF8String.t  (* the location of the error *)
                    * diagnosticseverity
                    (* * (int * string) option code and code description uri *)
                    * string (* brief message *)
                    * string option (* additional message *)

    type errlist = staticerror list

    datatype 'a witherrsoption = Success of 'a | DErrors of errlist | NotAvailable

    fun fmap (f : 'a -> 'b ) (x : 'a witherrsoption) : 'b witherrsoption = 
        case x of
            Success y => Success (f y)
            | DErrors l => DErrors l
            | NotAvailable  => NotAvailable

    exception NotSuccessErrors
    exception NotSuccessNotAvailble
    fun isSuccess  (x : 'a witherrsoption) : bool =
        case x of
            Success y => true
            | _ => false
    
    fun isNotSuccess  (x : 'a witherrsoption) : bool =
        not (isSuccess x)

    fun valOf  (x : 'a witherrsoption) : 'a =
        case x of
            Success y => y
            | DErrors l => raise NotSuccessErrors
            | NotAvailable => raise NotSuccessNotAvailble
    fun valOfSafe  (x : 'a witherrsoption) (default : errlist option ->  'a): 'a =
        case x of
            Success y => y
            | DErrors l => default (SOME l)
            | NotAvailable => default NONE
        
    fun next (x : 'a witherrsoption) (f : 'a -> 'b witherrsoption) : 'b witherrsoption = 
        case x of
            Success y => (f y)
            | DErrors l => DErrors l
            | NotAvailable  => NotAvailable
    fun >>= (x, f) = next x f
    infix 5 >>=
    (* this is guaranteed to fail when x fails, it collects all errors that handler might generate *)
    (* the handler is untouched if first thing succeeds *)
    fun failLookahead (x : 'a witherrsoption) (handler :  unit -> 'a witherrsoption) : 'a witherrsoption = 
        case x of
            Success y => Success(y)
            | DErrors l => (case handler() of 
                    Success _ => DErrors l 
                    | DErrors l2 => DErrors (l@l2)
                    | NotAvailable => NotAvailable
                )
            | NotAvailable  => NotAvailable
    fun >>/= (x, f) = failLookahead x f
    infix 5 >>/=
    fun >> (x, y) = next x (fn _ => y)
    infix 5 >>
    fun <$> (f, y) = fmap f y
    infix 5 <$>

    fun <?> (f, y) = failLookahead f y
    infix 5 <?>
    
    fun collectAlternatives (x : 'a witherrsoption list)  : 'a list witherrsoption = 
        let fun collectAlternativesRec (x : 'a witherrsoption list)  : 'a list = 
            case x of
                [] => []
                | (Success y :: xs) => y :: collectAlternativesRec xs
                | (DErrors l :: xs) => collectAlternativesRec xs
                | (NotAvailable :: xs) => collectAlternativesRec xs
        in 
            Success (collectAlternativesRec x)
        end

    fun collectAll (x : 'a witherrsoption list)  : 'a list witherrsoption = 
        let val res = 
            case x of
                [] => Success []
                | (Success y :: xs) => (case collectAll xs of 
                        Success ys => Success (y :: ys)
                        | DErrors l => DErrors l
                        | NotAvailable => NotAvailable
                        )
                | (DErrors l :: xs) => (case collectAll xs of 
                        Success ys => DErrors l
                        | DErrors l' => DErrors (l@l')
                        | NotAvailable => NotAvailable
                        )
                | (NotAvailable :: xs) => NotAvailable
        in 
            res
        end
    fun mapM (f : 'a -> 'b witherrsoption ) (l : 'a list) = 
        collectAll (map f l )

    fun genSingletonError(loc: UTF8String.t ) ( msghd : string ) (msgdetail : string option) : 'a witherrsoption =
        DErrors [ StaticError(loc, DiagnosticError, msghd, msgdetail) ]
    fun genSingletonErrorTuple(loc: UTF8String.t ) ( (msghd , msgdetail) : string * string option) : 'a witherrsoption =
        DErrors [ StaticError(loc, DiagnosticError, msghd, msgdetail) ]

    (* approximates (,) *)
    fun =/= ((t1, t2) : 'a witherrsoption * 'b witherrsoption) : ('a * 'b) witherrsoption 
        = t1 >>= (fn t1' => t2 >>= (fn t2' => Success (t1', t2')))
    (* approximates (,,) *)
    fun ==/= ((t1, t2, t3) : 'a witherrsoption * 'b witherrsoption* 'c witherrsoption) : ('a * 'b * 'c) witherrsoption 
        = t1 >>= (fn t1' => t2 >>= (fn t2' => t3  >>= (fn t3' => 
        Success (t1', t2', t3'))))
    (* approximates (,,,) *)
    fun ===/= ((t1, t2, t3, t4) : 'a witherrsoption * 'b witherrsoption* 'c witherrsoption * 'd witherrsoption) : ('a * 'b * 'c * 'd) witherrsoption 
        = t1 >>= (fn t1' => t2 >>= (fn t2' => t3  >>= (fn t3' =>  t4 >>= (fn t4' =>
        Success (t1', t2', t3', t4')))) )
end