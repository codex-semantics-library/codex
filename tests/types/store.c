

void test(void** a, void ** b) {
  *a = *b ;
  __VERIFIER_flush_cache() ;
}

void main(){}

