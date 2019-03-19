
int f(int x, int y, int z, int t) {
  if (x) {
    putchar(x);
    return f(y, z, t, x);
  }
  return 0;
}

int main() {
  f('A', 'B', 'C', 0);
  putchar(10);
  return 0;
}
