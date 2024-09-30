struct pair {
  int a ;
  int b ;
};

void test1(struct pair * ptr) {
  __VERIFIER_assert (ptr->a == ptr->b) ;
}

// /!\ p = pair? not pair+
void test2(struct pair * ptr) {
  struct pair p = *ptr ;
  if (ptr + 4 > 0) {
    int b = ptr->b ;
    __VERIFIER_assert (ptr->a == b) ; 
  }
}

void test3() {
  int i;
  int* value = &i;
  __VERIFIER_assert(value == 0) ;
}

void main() {}
