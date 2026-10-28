function sign(n) {
  if (n < 0) { return 9; }
  if (n > 0) { return 7; }
  return 5;
}
sign(0 - 3) * 100 + sign(4) * 10 + sign(0);
