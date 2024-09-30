//

void test(int * a, int * b) {
  int i = *a ;
  int j = *b ;
  //__VERIFIER_assert (j != 0) ;
  int c = i / j ;
  //__VERIFIER_assert (c) ;
}

void main(){}