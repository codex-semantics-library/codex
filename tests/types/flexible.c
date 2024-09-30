//

struct vectord {
    int len;  //short len ;  // Solve padding problems    // there must be at least one other data member
    int arr[]; // the flexible array member must be last
};

void read(struct vectord *p, int * res, int i) {
  if (0 <= i && i < p->len) {
    *res = p->arr[i] ;
  }
  __VERIFIER_flush_cache() ;
}

void read_wrong(struct vectord *p, int * res, int i) {
  if (0 <= i) {
    *res = p->arr[i] ;
  }
  __VERIFIER_flush_cache() ;
}


void write(struct vectord *p, int i, int v) {
  if (0 <= i && i < p->len) {
    p->arr[i] = v ;
  }
  __VERIFIER_flush_cache() ;
}

void iter(struct vectord *p, int * res) {
  for (int i = 0; i < p->len; i++) {
    *res = p->arr[i] ;
   p->arr[i] = 0 ;
  }
  __VERIFIER_flush_cache() ;
}

void main() {}
