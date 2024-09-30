void test_eq(int * a) {
	int c = *a ;
	__VERIFIER_assert(c == c) ;
	//__VERIFIER_flush_cache() ;
}

void test_eq2(int * a) {
	int c = *a ;
	int d = c ;
	__VERIFIER_assert(c == d) ;
	//__VERIFIER_flush_cache() ;
}

void test_eq_to_4(int * a) {
	__VERIFIER_assert(*a == 4) ;
	//__VERIFIER_flush_cache() ;
}

void test_load_eq(int * a) {
	int c = *a ;
	int d = *a ;
	__VERIFIER_assert(c == d) ;
	//__VERIFIER_flush_cache() ;
}

void main() {}

