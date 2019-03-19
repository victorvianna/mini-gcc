
int f(int x, int y, int z, int t) {
  if (!x) return 10;
  putchar(x);
  return f(y, z, t, x);
}

int main() {
  putchar(f('A', 'B', 'C', 0));
  return 0;
}
