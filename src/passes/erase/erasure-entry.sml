
structure ErasureEntry =
struct
open ErasurePass
    fun eraseSig (s : Signature) : PersistentKMachine.pkcomputation = 
                 #1 (eraseSigLazy (PreludeFunctions.erasurePrelude)  s)
    fun eraseSigK (s : Signature) : KMachine.kcomputation = 
               KMachineOps.toKComp  (KMachineOps.emptyCtx()) (eraseSig s)
end