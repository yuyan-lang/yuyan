

structure SpecialChars =
struct
  val leftSingleQuote = UTF8Char.fromString "「" NONE
  val rightSingleQuote = UTF8Char.fromString "」"  NONE
  val leftDoubleQuote =  UTF8Char.fromString "『" NONE
  val rightDoubleQuote = UTF8Char.fromString "』"  NONE
  val period = UTF8Char.fromString "。"  NONE
  val space = UTF8Char.fromString " "  NONE
  val newline = UTF8Char.fromString "\n"  NONE
  val tab = UTF8Char.fromString "\t"  NONE
end