void test(int ** a, int * b) {
  *a = b ;
  **a = *b ;
}

void test2(int * a) {
  *a = 4 ;
}

void main(){}
