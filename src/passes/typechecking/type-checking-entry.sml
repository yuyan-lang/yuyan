structure TypeCheckingEntry =
struct
open TypeCheckingPass
open StaticErrorStructure
  fun typeCheckSignatureTopLevel (s : RSignature) :CSignature witherrsoption =
    Success (#2 (typeCheckSignature PreludeFunctions.typeCheckingPrelude s []))
end
