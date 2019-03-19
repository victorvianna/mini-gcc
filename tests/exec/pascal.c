
/* triangle de Pascal modulo 7 */

struct List { int head; struct List *next; };

int get(struct List *l, int i) {
  if (i == 0) return l->head;
  return get(l->next, i-1);
}

int set(struct List *l, int i, int v) {
  if (i == 0) return l->head = v;
  return set(l->next, i-1, v);
}

struct List* create(int n) {
  struct List *r;
  if (n == 0) return 0;
  r = sbrk(sizeof(struct List));
  r->head = 0;
  r->next = create(n-1);
  return r;
}

int print_row(struct List *r, int i) {
  int j;
  j = 0;
  while (j <= i) {
    if (get(r, j) != 0)
      putchar('*');
    else
      putchar('.');
    j = j+1;
  }
  putchar(10);
  return 0;
}

int mod7(int x) {
  return x - 7*(x/7);
}

int compute_row(struct List *r, int i) {
  int j;
  j = i;
  while (j > 0) {
    set(r, j, mod7(get(r,j) + get(r,j-1)));
    j = j-1;
  }
  set(r, 0, 1);
  return 0;
}

int pascal(int n) {
  int i;
  struct List *r;
  r = create(n + 1);
  i = 0;
  while (i < n) {
    set(r, i, 0);
    compute_row(r, i);
    print_row(r, i);
    i = i+1;
  }
  return 0;
}

int main() {
  pascal(42);
  return 0;
}
