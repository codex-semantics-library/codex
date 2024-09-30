struct pair {
  int a;
  int b;
};

void test1(struct pair *p) {
  int a = p->b ;
  __VERIFIER_flush_cache () ;
}

void test2(struct pair *p) {
  struct pair a = *p ;
  __VERIFIER_flush_cache () ;
}

void main() {}
