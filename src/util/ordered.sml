
structure UTF8CharOrdered 
 :> ORDERED where type t = UTF8Char.t
 =
   struct
      type t = UTF8Char.t

      val ~= = UTF8Char.~=
      infix 4 ~=
      val eq : UTF8Char.t * UTF8Char.t -> bool = op =
      val compare :UTF8Char.t * UTF8Char.t -> order =  fn (c1, c2) => 
      if c1 ~= c2 then EQUAL else 
        if (UTF8Char.asUTF8WChar c1) < (UTF8Char.asUTF8WChar c2) then LESS else GREATER
   end


structure UTF8StringOrdered =ListOrdered(structure Ordered=UTF8CharOrdered)