
struct S { int a; int b; };

int main() {
  struct S *s;
  s = sbrk(sizeof(struct S));
  s->a = 'A';
  putchar(s->a);
  s->b = 'B';
  putchar(s->b);
  putchar(10);
  return 0;
}
