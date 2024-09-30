

void test(int * a, int * b) {
  int c = *a ;
  int d = *b ;
  __VERIFIER_assert(c == d) ;
  __VERIFIER_flush_cache() ;
}

void test2(void** a) {
	int c = *a ;
	int d = *a ;
	__VERIFIER_assert(c == d) ;
	__VERIFIER_flush_cache() ;
}

void main(){}

