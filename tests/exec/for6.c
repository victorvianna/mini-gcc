
int main() {
  int x;
  x = 10;
  while ((x = x-1) + 1) { putchar('A' + x); x = x-1; }
  putchar(10);
  return 0;
}
