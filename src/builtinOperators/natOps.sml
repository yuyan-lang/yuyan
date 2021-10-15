
structure NatOps =
struct
  val oneOp = Operators.parseOperator "一" false false 1000
  val twoOp = Operators.parseOperator "二" false false 1000
  val threeOp = Operators.parseOperator "三" false false 1000
  val plusOp = Operators.parseOperator "〇加〇" true true 900
  val minusOp = Operators.parseOperator "〇减〇" true true 900
  val timesOp = Operators.parseOperator "〇乘〇" true true 950
  val dividesOp = Operators.parseOperator "〇除以〇" true true 950
  val natOps = [ 
    oneOp
  ,plusOp
  ,twoOp, threeOp 
  ,minusOp, timesOp, dividesOp
  ]
end