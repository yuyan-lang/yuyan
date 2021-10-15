
structure GroupOps =
struct
  val groupOp = Operators.parseOperator "「〇」" false false 10000
  val groupOps = [ 
    groupOp
  ]
end