/*** listes circulaires doublement chaînées ***/

struct L {
  int valeur;
  struct L *suivant, *precedent;
};

/* liste réduite à un élément */
struct L* make(int v) {
  struct L* r;
  r = sbrk(sizeof(struct L));
  r->valeur = v;
  r->suivant = r->precedent = r;
  return r;
}

/* insertion après un élément donnée */
int inserer_apres(struct L *l, int v) {
  struct L *e;
  e = make(v);
  e->suivant = l->suivant;
  l->suivant = e;
  e->suivant->precedent = e;
  e->precedent = l;
  return 0;
}

/* suppression d'un élément donné */
int supprimer(struct L *l) {
  l->precedent->suivant = l->suivant;
  l->suivant->precedent = l->precedent;
  return 0;
}

/* affichage */
int afficher(struct L *l) {
  struct L *p;
  p = l;
  putchar(p->valeur);
  p = p->suivant;
  while (p != l) {
    putchar(p->valeur);
    p = p->suivant;
  }
  putchar(10);
  return 0;
}

/*** Partie 3 : problème de Josephus ***/

/* construction de la liste circulaire 1,2,...,n;
   l'élément renvoyé est celui contenant 1 */
struct L* cercle(int n) {
  struct L *l;
  int i;
  l = make(1);
  i = n;
  while (i >= 2) {
    inserer_apres(l, i);
    i = i-1;
  }
  return l;
}

/* jeu de Josephus */
int josephus(int n, int p) {
  /* c est le joueur courant, 1 au départ */
  struct L *c;
  c = cercle(n);

  /* tant qu'il reste plus d'un joueur */
  while (c != c->suivant) {
    /* on élimine un joueur */
    int i;
    i = 1;
    while (i < p) {
      c = c->suivant;
      i = i+1;
    }
    supprimer(c);
    c = c->suivant;
  }
  return c->valeur;
}

int print_int(int n) {
  int q;
  q = n / 10;
  if (n > 9) print_int(q);
  putchar('0' + (n - 10*q));
  return 0;
}

int main() {
  print_int(josephus(7, 5)); // 6
  putchar(10);
  print_int(josephus(5, 5)); // 2
  putchar(10);
  print_int(josephus(5, 17)); // 4
  putchar(10);
  print_int(josephus(13, 2)); // 11
  putchar(10);
  return 0;
}
