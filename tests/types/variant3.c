//#include "binsec-stubs.h"

union Lisp_object_union {
    void * list ;
    int other ;
};

void test(int ptr) {
  __VERIFIER_debug_int32((int) ptr) ;
  int tag = ptr & 7 ;
  int pointer = ptr & (~7) ;
  __VERIFIER_assert(((int) tag) < 8) ;
  __VERIFIER_assert(((int) pointer) != 0) ;
}

void main() {}
