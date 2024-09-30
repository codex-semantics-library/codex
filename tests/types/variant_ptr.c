


typedef union expr {
  int constant ;
  struct { int a ; int b ; } sum ;
} expr ;

typedef struct pair {
  int a ;
  int b ;
} pair ;

void test(int ptr) {
  if (((ptr << 29) >> 29) == 1) {
    expr * e = (expr*) ((ptr >> 3) << 3) ;
    __VERIFIER_assert((*e).constant < 1000) ;
  }
  else {
    expr * e = (expr*) ((ptr >> 3) << 3)  ;
    int a = (*e).sum.a ;
    int b = (*e).sum.b ;
    expr * e1 = (expr*) ((a >> 3) << 3)  ;
    expr * e2 = (expr*) ((b >> 3) << 3)  ;
    //__VERIFIER_assert (a->tag >= 0) ;
    //__VERIFIER_assert (b->tag >= 0) ;
    __VERIFIER_assert (0 < e1) ;
    __VERIFIER_assert (0 < e2) ;
  }
}

int main(void) {
  return 0;
}
