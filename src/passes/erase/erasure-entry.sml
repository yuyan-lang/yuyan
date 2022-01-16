
structure ErasureEntry =
struct
open ErasurePass
    (* fun eraseSig (s : Signature) : PersistentKMachine.pkcomputation = 
                  (eraseSigLazy NONE (PreludeFunctions.erasurePrelude)  s)
    fun eraseSigK (s : Signature) : KMachine.kcomputation = 
               KMachineOps.toKComp  (KMachineOps.emptyCtx()) (eraseSig s) *)
end