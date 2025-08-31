
Run `dune build` to test compilation.

To test the standard library: run `dune exec -- yyocamlc 藏书阁/标准库。豫`

「：：」 are comment markers. 

`术「399」「左」加「右」「400」盖谓「加」于「左」于「右」也。` declares a custom infix left-associative operator. 
It defines `A加B` as the expression `「加」(A, B)` (function `加` applied to `A` and `B`). The leading 
and trialing numbers are precedence when VIEWED from either left or right and here it has higher 
precedence when viewed from right (so in `A加B加C`, `A` and `B` are grouped as it has 400 precedence 
when viewed from `C` but `B` and `C` only has 399 precedence when viewed from `A` ).
If you want to have prefix or postfix operators, you may use the following:
- `术一加「右」「400」盖谓「加」于一于「右」也。` (prefix: 一加B)
- `术「400」「左」加一谓「加」于「左」于一也。` (postfix: A加一)
- `术一加「中」加一盖谓「加」于「中」于二也。` (circumfix: 一加C加一)

