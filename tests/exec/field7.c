
struct S { int a; int b; };

int main() {
  struct S *s;
  struct S *p;
  s = sbrk(sizeof(struct S));
  p = s;
  p->a = 'A';
  putchar(s->a);
  putchar(p->a);
  p->b = 'B';
  putchar(s->b);
  putchar(p->b);
  putchar(10);
  return 0;
}
