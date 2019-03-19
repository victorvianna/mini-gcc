int print_int(int n) {
  int q;
  q = n / 10;
  if (n > 9) print_int(q);
  putchar('0' + (n - 10*q));
  return 0;
}

int main() {
  print_int(42);
  putchar(10);
  return 0;
}
