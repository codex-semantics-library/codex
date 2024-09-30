struct a d;
struct a {
  struct a *b;
  struct a *c;
};

void main() {}

void test(int x, struct a *f) {
  struct a a = *f;
  d = *a.b;
}
