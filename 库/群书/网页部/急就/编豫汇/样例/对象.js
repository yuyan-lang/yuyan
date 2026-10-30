var o = { x: 3, y: 4 };
var s = o.x + o.y;
o.x = 10;
var t = 0;
if (o) { t = t + 1; }
s + o.x + t;
