
int main() {
  int n;
  n = 0;
  { int n;
    n = 1;
    if (n == 1)
      putchar('a');
  }
  if (n == 0)
    putchar('b');
  putchar(10);
  return 0;
}
