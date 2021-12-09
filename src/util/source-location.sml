
structure SourceLocation = struct

    (* sourcelocation is file path , line, col *)
    type sourcelocation = string * int * int
    type t = sourcelocation
end