
int main() {
  int x, y;
  x = 65;
  putchar(x);
  putchar(x = x+1);
  putchar(y = x+1);
  putchar(10);
  return 0;
}
