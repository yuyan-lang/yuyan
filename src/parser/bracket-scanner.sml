structure BracketScanner =
struct
    (* scans until next corresponding quote, returned string includes the quote *)
    (* The returned boolean require the function to be called with either 0 1 or 1 0 as arguments *)
    (* return true true if seen both quotes and periods inside, otherwise false *)
    fun scanUntilCorrespondingRightQuote (curSeenLeftSingleQuote : int)
        (curSeenLeftDoubleQuote: int) : UTF8String.t -> (UTF8String.t * UTF8String.t * bool * bool) =
        fn s => 
            let fun go (remaining : UTF8String.t) (sofar : UTF8String.t) 
            (curSeenLeftSingleQuote : int) (curSeenLeftDoubleQuote: int) 
            (totalSeenRightSingleQuote : int) (totalSeenRightDoubleQuote: int) 
            (seenInnerPeriods : bool) : (UTF8String.t * UTF8String.t * bool * bool) = 
            if curSeenLeftSingleQuote = 0 andalso curSeenLeftDoubleQuote = 0
            then (sofar, remaining, totalSeenRightSingleQuote + totalSeenRightDoubleQuote >1, seenInnerPeriods)
            else (case remaining of
                [] => (sofar, [], totalSeenRightSingleQuote + totalSeenRightDoubleQuote >1, seenInnerPeriods)
                | (h :: t) => 
                              if h = SpecialChars.rightSingleQuote
                              then go t (sofar @[h]) (curSeenLeftSingleQuote - 1) (curSeenLeftDoubleQuote) (totalSeenRightSingleQuote+1) totalSeenRightDoubleQuote seenInnerPeriods
                              else
                              if h = SpecialChars.rightDoubleQuote
                              then go t (sofar @[h]) (curSeenLeftSingleQuote) (curSeenLeftDoubleQuote-1) totalSeenRightSingleQuote (totalSeenRightDoubleQuote+1) seenInnerPeriods
                              else
                              if h = SpecialChars.leftSingleQuote
                              then go t (sofar @[h]) (curSeenLeftSingleQuote + 1) (curSeenLeftDoubleQuote) totalSeenRightSingleQuote totalSeenRightDoubleQuote seenInnerPeriods
                              else
                              if h = SpecialChars.leftDoubleQuote
                              then go t (sofar @[h]) (curSeenLeftSingleQuote) (curSeenLeftDoubleQuote+1) totalSeenRightSingleQuote totalSeenRightDoubleQuote seenInnerPeriods
                              else
                              if h = SpecialChars.period
                              then go t (sofar @[h]) (curSeenLeftSingleQuote) (curSeenLeftDoubleQuote) totalSeenRightSingleQuote totalSeenRightDoubleQuote true
                              else go t (sofar @[h]) (curSeenLeftSingleQuote) (curSeenLeftDoubleQuote) totalSeenRightSingleQuote totalSeenRightDoubleQuote seenInnerPeriods

            ) 
        in go s [] curSeenLeftSingleQuote curSeenLeftDoubleQuote 0 0 false
        end
end