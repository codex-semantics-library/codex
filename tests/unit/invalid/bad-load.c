#include "binsec-stubs.h"

struct pair {
  int a;
  int b;
}

test1(struct pair *p) {
  int a = p->b ;
  __VERIFIER_flush_cache () ;
}

test2(struct pair *p) {
  struct pair a = *p ;
  __VERIFIER_flush_cache () ;
}

main() {}
