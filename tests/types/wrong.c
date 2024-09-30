#include "binsec-stubs.h"

struct pair {
  int a ;
  int b ;
}

void test1(pair * ptr) {
  __VERIFIER_assert (ptr->a == ptr->b) ;
}

// /!\ p = pair? not pair+
void test2(pair * ptr) {
  pair p = ptr* ;
  if (ptr + 4 > 0) {
    int b = ptr->b ;
    __VERIFIER_assert (ptr->a == b) ; 
  }
}

void test3 () {
  int i;
  int* value = &i;
  __VERIFIER_assert(value == 0) ;
}

int main() {
  return 0;
}
