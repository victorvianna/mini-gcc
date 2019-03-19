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

int main() {
  struct L *l;
  l = make(65);
  afficher(l);
  inserer_apres(l, 66);
  afficher(l);
  inserer_apres(l, 67);
  afficher(l);
  supprimer(l->suivant);
  afficher(l);
  return 0;
}
