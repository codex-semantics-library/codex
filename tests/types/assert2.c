void assert_plus10(int * a) {
	int i = *a ;
	int res = i + 10 ;
	__VERIFIER_assert (res) ;
}

void assert_eq(void ** a, void ** b) {
  __VERIFIER_assert(a == b) ;
}

void assert_assign(void ** a) {
	int c = *a ;
	int d = *a ;
	__VERIFIER_assert(c == d) ;
	__VERIFIER_flush_cache() ;
}

void main() {}
