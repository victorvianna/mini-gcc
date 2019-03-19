struct S { int a; };

int main() {
  struct S *p;
  p = 0;
  putchar(p->a);
  return 0;
}
