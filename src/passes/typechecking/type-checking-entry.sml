structure TypeCheckingEntry =
struct
open TypeCheckingPass
open StaticErrorStructure
  fun typeCheckSignatureTopLevel (s : RSignature) :CSignature witherrsoption =
  let val res = (#2 (typeCheckSignature PreludeFunctions.typeCheckingPrelude s []))
    (* val _ = DebugPrint.p "Type checked top level\n"
    val _ = DebugPrint.p (PrettyPrint.show_typecheckingCSig res) *)
  in Success res end
end
