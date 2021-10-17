structure PrettyPrint 
 =
struct


  fun show_rawast (x : RawAST.RawAST) : string = let 
    open RawAST
    in case x of 
       RawID s => UTF8Char.toString s
      end
  fun show_rawastsPlain (l : RawAST.RawAST list) =  (String.concat (map show_rawast l))
  fun show_rawasts (l : RawAST.RawAST list) = "("^ (String.concatWith ", " (map show_rawast l)) ^")" 

  fun show_opcomptype (x : Operators.opComponentType) :string = let 
    open Operators
    in
      case x of
        OpCompExpr => UTF8Char.toString underscoreChar
        | OpCompBinding => UTF8Char.toString bindingChar
        | OpCompString s => UTF8String.toString s
    end


  fun show_op (x : Operators.operator) = let 
    open Operators
    in case x of
      Operator(p, fix, assoc, comps, uid) =>
      let val baseName = String.concat (map show_opcomptype comps)
        val underscoreChar = UTF8Char.toString underscoreChar
      in case fix of
          Prefix =>  baseName ^ underscoreChar 
          | Infix => underscoreChar ^ baseName ^ underscoreChar 
          | Postfix =>underscoreChar ^ baseName
          | Closed => baseName
        end
    end

  fun show_opast (x : Operators.OpAST) = let 
    open Operators
    in case x of 
      OpAST (oper, l) => (show_op oper) ^ "[" ^ String.concatWith ", " (map show_opast l) ^ "]"
      | UnknownOpName s => "?[" ^ UTF8String.toString s ^ "]"
      | NewOpName s => "![" ^ UTF8String.toString s ^ "]"
    end



fun show_parseopast x = let 
open ParseAST in 
case x of
    ParseOpAST (r, l) => show_parseRule r ^ "[" ^ (String.concatWith ", " (map show_parseopast l))  ^"]" 
    end

and show_parseRule x = let 
open Operators 
open ParseAST
in
case x of
    OperatorNameComponent (f,oper) => "OperatorNameComponent " ^ UTF8String.toString f ^  " in " ^ show_op oper
    | OperatorInternal oper => "OperatorInternal " ^ show_op oper
    | PrefixNoneAssoc oper=> "PrefixNoneAssoc "^ show_op oper
    | PrefixRightAssoc oper=> "PrefixRightAssoc "^ Int.toString(oper)
    | PostfixNoneAssoc oper=> "PostfixNoneAssoc "^ show_op oper
    | PostfixLeftAssoc oper=> "PostfixLeftAssoc "^ Int.toString(oper)
    | InfixNoneAssoc oper=> "InfixNoneAssoc "^ show_op oper
    | InfixLeftAssoc oper=> "InfixLeftAssoc "^ Int.toString(oper)
    | InfixLeftAssocLeftArrow oper=> "InfixLeftAssocLeftArrow "^ show_op oper
    | InfixRightAssoc oper=> "InfixRightAssoc "^ Int.toString(oper)
    | InfixRightAssocRightArrow oper=> "InfixRightAssocRightArrow "^ show_op oper
    | Many1  => "Many1"
    | EOF  => "EOF"
    | ExpWithEOF  => "ExpWithEOF"
    | UnknownId  => "UnknownId"
    | UnknownIdComp s => "UnknownIdComp "^ show_rawast s
    | Binding id => "Binding "^ show_rawastsPlain id
    | QuotedName s => "QuotedName "^UTF8String.toString s
end
end