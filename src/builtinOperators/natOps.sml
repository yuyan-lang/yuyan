
structure NatOps =
struct
  val oneOp = Operators.parseOperatorStr "一" false false 1000 []
  val twoOp = Operators.parseOperatorStr "二" false false 1000 []
  val threeOp = Operators.parseOperatorStr "三" false false 1000 []
  val plusOp = Operators.parseOperatorStr "〇加〇" true true 900 []
  val minusOp = Operators.parseOperatorStr "〇减〇" true true 900 []
  val timesOp = Operators.parseOperatorStr "〇乘〇" true true 950 []
  val dividesOp = Operators.parseOperatorStr "〇除以〇" true true 950 []
  val negateOp = Operators.parseOperatorStr "负〇" false false 980 []
  val natOps = [ 
    oneOp
    , negateOp
  ,plusOp
  ,twoOp, threeOp 
  ,minusOp, timesOp, dividesOp
  ]
end