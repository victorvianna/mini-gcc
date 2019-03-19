
struct U { int x; int y; };

struct S { int a; struct U *u; int b; };

int main() {
  struct U *u;
  struct S *s;

  u = sbrk(sizeof(struct U));
  s = sbrk(sizeof(struct S));
  s->u = sbrk(sizeof(struct U));
  s->a = 'A';
  s->b = 'B';
  s->u->x = 'x';
  s->u->y = 'y';
  putchar(s->a);
  putchar(s->u->x);
  putchar(s->u->y);
  putchar(s->b);
  putchar(10);
  u->x = 'X';
  u->y = 'Y';
  putchar(s->a);
  putchar(s->u->x);
  putchar(s->u->y);
  putchar(s->b);
  putchar(10);
  s->u = u;
  putchar(s->a);
  putchar(s->u->x);
  putchar(s->u->y);
  putchar(s->b);
  putchar(10);
  return 0;
}
