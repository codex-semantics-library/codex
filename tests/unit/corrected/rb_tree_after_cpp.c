#include "binsec-stubs.h"

struct a {
  struct a *b;
  struct a *c
} f, *g;

struct d {
  struct a *e
}

main() {}

main_insert(struct d *h, __attribute__(()) j) {
  struct d i = *h;
  struct a *a = j;
  f = *i.e;
  g = f.b;
  a->c = NULL;
  if (f.b)
    g->b = j;
}
