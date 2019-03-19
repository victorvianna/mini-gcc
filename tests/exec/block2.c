
int main() {
  int x;
  x = 65;
  putchar(x);
  if (0) {
    int y;
    y = 66;
    putchar(y);
  } else {
    int y, z;
    y = 67;
    z = 68;
    putchar(y);
    putchar(z);
  }
  putchar(x);
  putchar(10);
  return 0;
}
