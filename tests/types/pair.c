struct pair {
  int x;
  int y;
} ;

struct pair_of_pair {
  struct pair * p1 ;
  struct pair * p2 ;
} ;

void test_pair_eq(struct pair * a) {
	int c = a->x ;
	int d = a->y ; 
	__VERIFIER_assert(c == d) ;
	__VERIFIER_flush_cache() ;
}

void test_pair_of_pair_eq(struct pair_of_pair * a) {
	int c = a->p1->x ;
	int d = a->p2->y ; 
	__VERIFIER_assert(c == d) ;
	__VERIFIER_flush_cache() ;
}

void test_pair_of_pair_eq2(struct pair_of_pair * a) {
	struct pair * p = a->p1 ;
	int c = p->x ;
	int d = p->y ; 
	__VERIFIER_assert(c == d) ;
	__VERIFIER_flush_cache() ;
}

void test_pair_of_pair_eq3(struct pair * a, struct pair * b) {
	a->x = 1 ;
	a->y = 4 ;
	a->x = b->x ;
	int c = a->x ;
	int d = a->y ; 
	__VERIFIER_assert(c == d) ;
	__VERIFIER_flush_cache() ;
}

void write_1_x_4(struct pair * a) {
	a->x = 1 ;
	a->y = 4 ;
	__VERIFIER_flush_cache() ;
}

void main() {}
