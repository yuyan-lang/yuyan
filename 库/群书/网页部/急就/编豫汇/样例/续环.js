var t = 0;
for (var j = 1; j <= 10; j++) {
  if (j === 3) { continue; }
  if (j > 6) { break; }
  t = t + j;
}
t;
