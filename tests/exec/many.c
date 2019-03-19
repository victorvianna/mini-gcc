int many(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j) {
  putchar(64 + a);
  putchar(64 + b);
  putchar(64 + c);
  putchar(64 + d);
  putchar(64 + e);
  putchar(64 + f);
  putchar(64 + g);
  putchar(64 + h);
  putchar(64 + i);
  putchar(64 + j);
  putchar(10);
  if (a < 10)
    many(b, c, d, e, f, g, h, i, j, a);
  return 0;
}
int main() {
  many(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  return 0;
}
