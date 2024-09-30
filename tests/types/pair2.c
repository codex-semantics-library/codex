struct pair {
  int x;
  int y;
};

void test_const1(struct pair *p) {
  p->x = 4 ;
  p->y = 2 ;
  __VERIFIER_flush_cache() ;
}

void test_const2(struct pair *p) {
  p->x = 2 ;
  p->y = 2 ;
  __VERIFIER_flush_cache() ;
}


void test_exist1(struct pair *p) {
  int x = 3 ;
  p->x = 3 ;
  p->y = 3 ;
  __VERIFIER_flush_cache() ;
}

void test_exist2(struct pair *p, int * a) {
  p->x = *a ;
  p->y = *a ;
  __VERIFIER_flush_cache() ;
}

void test_exist3(struct pair *p, int * a, int * b) {
  p->x = *a ;
  p->y = *b ;
  __VERIFIER_flush_cache() ;
}

void main() {}
