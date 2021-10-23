
structure ErasureEntry =
struct
open ErasurePass
    fun eraseSig (s : Signature) : PersistentKMachine.pkcomputation = 
                 PersistentKMachine.fromKComp 
                 (eraseSigLazy (PreludeFunctions.erasurePrelude)  s)
    fun eraseSigK (s : Signature) : KMachine.kcomputation = 
               PersistentKMachine.toKComp  (PersistentKMachine.emptyCtx()) (eraseSig s)
end