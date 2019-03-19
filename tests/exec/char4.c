
int f(int x, int y) {
  return x+y;
}

int main() {
  int c;
  putchar(f('A', 0));
  putchar(f('A', 1));
  putchar(f('A', 2));
  c = f('A', 3);
  putchar(c);
  c = c+1;
  putchar(c);
  putchar(10);
  return 0;
}
