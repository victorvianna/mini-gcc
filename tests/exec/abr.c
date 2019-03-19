/* arbres binaires de recherche */

struct ABR {
  int valeur;
  struct ABR *gauche, *droite;
};

struct ABR* make(int v, struct ABR *g, struct ABR *d) {
  struct ABR * s;
  s = sbrk(sizeof(struct ABR));
  s->valeur = v;
  s->gauche = g;
  s->droite = d;
  return s;
}

int insere(struct ABR *a, int x) {
  if (x == a->valeur)
    return 0;
  if (x < a->valeur) {
    if (a->gauche == 0)
      a->gauche = make(x, 0, 0);
    else
      insere(a->gauche, x);
  } else
    if (a->droite == 0)
      a->droite = make(x, 0, 0);
    else
      insere(a->droite, x);
  return 0;
}

int contient(struct ABR *a, int x) {
  if (x == a->valeur) return 1;
  if (x < a->valeur && a->gauche != 0) return contient(a->gauche, x);
  if (a->droite != 0) return contient(a->droite, x);
  return 0;
}

int print_int(int n) {
  int q;
  q = n / 10;
  if (n > 9) print_int(q);
  putchar('0' + (n - 10*q));
  return 0;
}

int print(struct ABR *a) {
  putchar('(');
  if (a->gauche != 0) print(a->gauche);
  print_int(a->valeur);
  if (a->droite != 0) print(a->droite);
  return putchar(')');
}

int main() {
  struct ABR *dico;
  dico = make(1, 0, 0);
  insere(dico, 17);
  insere(dico, 5);
  insere(dico, 8);
  print(dico);
  putchar(10);
  if (contient(dico, 5) && !contient(dico, 0) &&
      contient(dico, 17) && !contient(dico, 3)) {
    putchar('o');
    putchar('k');
    putchar(0x0a);
  }
  insere(dico, 42);
  insere(dico, 1000);
  insere(dico, 0);
  print(dico);
  putchar(10);
  return 0;
}

