structure SourceRange = struct

    (* sourcerange is file path , line, col, endLine, endCol *)
    datatype sourcerange = StartEnd of string * int * int * int * int
                          (* | StartLength of string * int * int * int start line col length *)
    type t = sourcerange
end