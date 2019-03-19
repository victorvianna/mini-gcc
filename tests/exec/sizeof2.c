
struct S { int a; };
struct T { int a; struct S *p; };

int main() {
  putchar(65 + sizeof(struct S));
  putchar(10);
  putchar(65 + sizeof(struct T));
  putchar(10);
  return 0;
}

