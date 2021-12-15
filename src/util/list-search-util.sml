
structure ListSearchUtil = struct

exception NotFound of string


fun lookup (l : (string *'a ) list) (key : string) : 'a = 
    case l of
        [] => raise NotFound key
        | ((k, v) :: t) => if k = key then v else lookup t key

end