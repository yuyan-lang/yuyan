structure TypeCheckingEntry =
struct
open TypeCheckingPass
  fun typeCheckSignatureTopLevel (s : RSignature) :CSignature =
    (#2 (typeCheckSignature PreludeFunctions.typeCheckingPrelude s []))
end
