var o = { a: 0, b: 0 };
for (var i = 1; i <= 1000; i++) {
  o.a = o.a + i;
  o.b = o.b + i * 2;
}
o.a + o.b;
