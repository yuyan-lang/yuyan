structure TypeCheckingEntry =
struct
open TypeCheckingPass
open StaticErrorStructure
  fun typeCheckSignatureTopLevel (s : RSignature) :CSignature witherrsoption =
  let val res =  (typeCheckSignature 
  (TypeCheckingPass.Context (StructureName.topLevelName, true, 
        []))
  s [])
    (* val _ = DebugPrint.p "Type checked top level\n"
    val _ = DebugPrint.p (PrettyPrint.show_typecheckingCSig res) *)
  in fmap (#2) res end
end
