void test(int * a, int * b) {
  *a = *b ;
  __VERIFIER_assert (*a == 3) ;
  //__VERIFIER_flush_cache () ;
}

void main(){}

