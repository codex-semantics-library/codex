#include "binsec-stubs.h"

typedef struct expr {
  int tag ;
  union {
    int constant ;
    struct { struct expr * a ; struct expr * b ; } sum ;
  } data ;
} expr ;

void test(struct expr * e) {
	if (e->tag == 0) {
	  __VERIFIER_assert(e->tag == 0) ;
	  __VERIFIER_assert(e->data.constant < 1000) ;
	}
	else {
	  __VERIFIER_assert(e->tag != 0) ;
	  expr * a = e->data.sum.a ;
	  expr * b = e->data.sum.b ;
	  //__VERIFIER_assert (a->tag >= 0) ;
	  //__VERIFIER_assert (b->tag >= 0) ;
	  __VERIFIER_assert (0 < a) ;
	  __VERIFIER_assert (0 < b) ;
	}
}

int main(void) {
  return 0;
}
