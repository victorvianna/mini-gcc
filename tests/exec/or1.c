
int main() {
  putchar(65 + (1 || 1)); // 66, pas 67 !
  putchar(65 + (0 || 2)); // 66, pas 67 !
  putchar(65 + (1 || 0));
  putchar(65 + (0 || 0));
  putchar(10);
  return 0;
}
