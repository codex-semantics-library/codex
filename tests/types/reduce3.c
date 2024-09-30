struct a {
  struct a *b;
  struct a *c;
} f, *g;

struct d {
  struct a *e;
};

void main() {}

void test(struct d *h, __attribute__(()) j) {
  struct d i = *h;
  struct a *a = j;
  f = *i.e;
  g = f.b;
  a->c = 0;
  if (f.b)
    g->b = j;
}
