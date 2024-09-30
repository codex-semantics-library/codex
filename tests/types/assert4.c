void test_eq(int * a, int * b) {
	int c = *a ;
	int d = *b ;
	__VERIFIER_assert(c == d) ;
	//__VERIFIER_flush_cache() ;
}

void test_diff(int * a, int * b) {
	int c = *a ;
	int d = *b ;
	__VERIFIER_assert(c != d) ;
	//__VERIFIER_flush_cache() ;
}

void main() {}

