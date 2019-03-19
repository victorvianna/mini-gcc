int fact_rec(int n) {
  if (n <= 1) return 1;
  return n * fact_rec(n - 1);
}

int main() {
  if (fact_rec(0) == 1) putchar('1');
  if (fact_rec(1) == 1) putchar('2');
  if (fact_rec(5) == 120) putchar('3');
  putchar(10);
  return 0;
}
