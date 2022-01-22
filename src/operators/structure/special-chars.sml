

structure SpecialChars =
struct
  val leftSingleQuote = UTF8Char.fromString "「" NONE
  val rightSingleQuote = UTF8Char.fromString "」"  NONE
  val leftDoubleQuote =  UTF8Char.fromString "『" NONE
  val rightDoubleQuote = UTF8Char.fromString "』"  NONE
  val leftAngledBracket =  UTF8Char.fromString "《" NONE
  val rightAngledBracket = UTF8Char.fromString "》"  NONE
  val leftParenthesis = UTF8Char.fromString "（" NONE
  val rightParenthesis = UTF8Char.fromString "）" NONE
  val period = UTF8Char.fromString "。"  NONE
  val space = UTF8Char.fromString " "  NONE
  val newline = UTF8Char.fromString "\n"  NONE
  val tab = UTF8Char.fromString "\t"  NONE
  val tilde = UTF8Char.fromString "~"  NONE
  val slash = UTF8Char.fromString "/"  NONE
  val pathSeparator = slash
end