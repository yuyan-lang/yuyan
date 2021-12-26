signature FILE_RESOURCE_URI =
sig
type t
val make : string -> t
val access : t -> string
exception NotAbsolute of string

(* structure FileResourceURIOrdered : ORDERED  *)

end
structure FileResourceURI :> FILE_RESOURCE_URI=
struct
    datatype fileresourceuri = FRPath of string
    type t = fileresourceuri

    exception NotAbsolute of string

    fun make (path : string) = 
    if OS.Path.isAbsolute path
    then FRPath path
    else raise NotAbsolute path
    fun access (FRPath path : fileresourceuri) =  path

    (* structure FileResourceURIOrdered 
   :> ORDERED where type t = fileresourceuri
   =
   struct

      type t =  fileresourceuri

      fun eq (FRPath p1, FRPath p2) =
            p1 = p2

      fun compare (FRPath p1, FRPath p2) =
        String.compare(p1,p2)

   end *)
end


