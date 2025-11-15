I use md because of keybindings don't work for txt files. Treat this as a txt file.

Update: I decided that prefix operators should be all ordered below
postfix and infix.
So once we parse any prefix operator, we know we have determined the parsing patterns 
for the next bit.
in this way, a decision >< is unambiguous between:
Prefix and Infix 
Prefix and Postfix
Only priority is needed for Infix <-> Infix, and Infix <-> Postfix pairs
(* you only need 
to declare priority if you are infix or postfix, and by default, infix operators associates to 
the left for type checking efficiency purposes, so no need to declare any fixity now
(with backtracking to fix to the right)
( I think by default I am going to make postfix of lower precedence 
than infix, but also with backtracking)
However, if fixity is declared they are strictly followed without backtracking
*)


Name ?LP ?RP
内建（）   2000
《《外部调用》》（） 2000
（）之（） 999 1000 (* left assoc *)
（）点（） 1000 1000 (* decimal points)
（）于（） 799 800

（）之书   90
观（）     80
诵（）     80
其（）者（） 20
（）、（） 109 110
（），（） 49 50
若（）则（）否则（）80
有（）携（）70
（）或（）59 60
虑「」者（）而（）55
会「」而（） 50
会（）者「」而（） 50
化（）者「」而（）40
化（）而（）40
（）者（）也 10 10

StdLib

（）且（） 340 339


Other Reductions
（）。   Reduces anything >= 0