//#include "binsec-stubs.h"

void test(int * a) {
	//int n = a + 4 ;
	int n = (*a) ;
	if (0 < n) __VERIFIER_assert (0 < n) ;
	else __VERIFIER_assert (0 >= n) ;
}

void main() {}
