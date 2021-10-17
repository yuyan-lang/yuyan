
structure GroupOps =
struct
(* should build quotes inside parser as we need to distinguish names from expressions! *)
  val groupOp = Operators.parseOperator "「〇」" false false 10000 []
  val sepOp = Operators.parseOperator "〇。〇" true false 0 []
  val endOp = Operators.parseOperator "〇。" true false (0-1) []
  val quoteOp = Operators.parseOperator "『〇』" true false (10000) [1]
  val groupOps = [ 
    groupOp
    , 
    sepOp
    , endOp
    , quoteOp
  ]
end