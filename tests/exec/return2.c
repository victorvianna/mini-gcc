
int f(int x, int y) {
  return x + 2*y;
}

int main() {
  putchar(f('A', 0));
  putchar(f('A', 1));
  putchar(f('A', 2));
  putchar(10);
  return 0;
}
