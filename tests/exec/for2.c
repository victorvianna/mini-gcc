int main() {
  int i;
  i = 10;
  while (i > 0)
    putchar('A' + (i = i-1) + 1);
  putchar(10);
  return 0;
}
