var s = "";
var t = "x";
var r = 0;
if (s) { r = r + 1; } else { r = r + 2; }
if (t) { r = r + 3; } else { r = r + 4; }
while (s) { r = r + 100; }
if (!s) { r = r + 5; }
if (!t) { r = r + 6; }
r;
