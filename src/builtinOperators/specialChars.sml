

structure SpecialChars =
struct
  val leftSingleQuote = UTF8Char.fromString "「"
  val rightSingleQuote = UTF8Char.fromString "」" 
  val leftDoubleQuote =  UTF8Char.fromString "『"
  val rightDoubleQuote = UTF8Char.fromString "』" 
  val period = UTF8Char.fromString "。" 
  val space = UTF8Char.fromString " " 
  val newline = UTF8Char.fromString "\n" 
  val tab = UTF8Char.fromString "\t" 
end