# Syntax Sheet (Informal)
The key words (that cannot appear as a name and has a special meaning no matter where it is) are
  `"「", "」", "『", "』", "。"`. The single quotes are used for identifiers (if 
  no keywords appear inside a pair of `"「", "」"`) and brackets (if there 
  are keywords appearing inside the bracket).

| 豫言           | Standard Language | 
| ------------- |-------------| 
| 〇者〇也 |  type t = T |
| 以〇为〇 |  e : T | 
| 设〇为〇 |  #define e = E |
| 施〇乃为〇 | e = E |
| 术〇交[左右无]序[零一二三四五六七八九]+也 | infix[lr ] op [0123456789]+ | 
| ------------- |-------------| 
| 有 | 1 |
| 无 | 0 | 
  |夫〇表〇| l : t |
  |〇合〇 | *  | 
  |〇亦〇 | +  |
  |夫〇表〇合夫〇表〇| *{l : t, l2 : t2} |
  |夫〇表〇亦夫〇表〇| +{l : t, l2 : t2} |
  |化〇而〇 | t1 -> t2 | 
|承〇而〇 | ∀t. T |
|有〇则〇 | ∃t. T |
 |复〇为〇 | ρt. T |
| ------------- |-------------| 
|元| () |
|〇之〇| e1.l |
 |〇于〇| (e1 e2) |
 |〇与〇| ⟨e1, e2, e3⟩ |
 |〇临〇| l.e |
   |卷〇| fold e|
   |舒〇| unfold e| 
   |鉴〇而「曰〇则有〇而〇[或〇曰〇则有〇而〇]+」| case e of {l.x => e1 \| l2.y => e2} |
   |授〇以〇| e [T] |
   |入〇合〇| pack t in e |
   |开〇则有〇者〇而〇| open e as t, x in e2 |
|会〇而〇| λ x. e |
|遇〇者〇而〇| λ x: T. e|
|受〇而〇| Λ t. e|