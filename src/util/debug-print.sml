structure DebugPrint =
struct
  fun p (s : string) :unit = 
    (TextIO.output (TextIO.stdErr, s); TextIO.flushOut TextIO.stdErr)
end
