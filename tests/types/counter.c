//

void incr(int * a) {
	int length = *a ;
	int n = 0 ;
	for (int i = 0; i < length; i++) {
	  __VERIFIER_assert(n == i) ;
	  n++;
	}
	//for (int i = 0 ; i < length; i = i + 1) ;
	//__VERIFIER_assert(n == length) ;
}

void incr2(int * a, int * b) {
	int length = *a ;
	int init = *b ;
	int n = init ;
	for (int i = 0; i < length; i++) {
	  __VERIFIER_assert((init + i) == n) ;
	  n++;
	}
	//for (int i = init ; i < length; i = i + 1) ;
	//__VERIFIER_assert(n == length) ;
}

void decr(int * a) {
	int length = *a ;
	//int n = 0 ;
	/* for (int i = length; i >= 0; i--) {
		__VERIFIER_assert(n == length - i) ;
		n++ ;
	}
	__VERIFIER_assert(n == length) ;*/
	for (int i = 0 ; i >= length; i = i - 4) ;
}

void decr2(int * a, int * b) {
	int length = *a ;
	int init = * b ;
	//int n = 0 ;
	/* for (int i = length; i >= 0; i--) {
		__VERIFIER_assert(n == length - i) ;
		n++ ;
	}
	__VERIFIER_assert(n == length) ;*/
	for (int i = init ; i >= length; i = i - 4) ;
}

void main() {}
