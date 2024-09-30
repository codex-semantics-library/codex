

void test(int * vect) {
	int length = (*vect) ;
	vect++;
	for (int i = 0; i < length; i++) {
		//__VERIFIER_assert (vect + i == ptr) ;
		*(vect + i) = 47 ;
	}
	//__VERIFIER_flush_cache() ;
}

void main() {}

