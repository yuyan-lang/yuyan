structure TypeCheckingEntry =
struct
open TypeCheckingPass
  fun typeCheckSignatureTopLevel (s : Signature) :unit =
    (typeCheckSignature PreludeFunctions.typeCheckingPrelude s; ())
end
