int main() {
  int i;
  int cpt;
  cpt = 0;
  i = 0;
  while (i < 10) {
    int j;
    j = 10;
    while (j > 0) {
      cpt = cpt+1;
      j = j-1;
    }
    i = i+1;
  }
  if (cpt == 100)
    putchar('!');
  putchar(10);
  return 0;
}
