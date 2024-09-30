#include "binsec-stubs.h"

void test_ge(int * a, int * b) {
	int i = *a ;
	int j = *b ;
	if (i >= j) {
	  __VERIFIER_assert (i >= j) ;
	}
}

void test_lt20(int * num) {
	int i = *num ;
	if (i < 20) {
	  __VERIFIER_assert (i < 20) ;
	}
}

void test_gt(int * a, int * b) {
	int i = *a ;
	int j = *b ;
	if (i > j) {
	  __VERIFIER_assert (i > j) ;
	} else { __VERIFIER_assert (i <= j) ; }
}

void test_gt20(int * num) {
	int i = *num ;
	if (i > 20) {
	  __VERIFIER_assert (i > 20) ;
	}
}

void test_le(int * a, int * b) {
	int i = *a ;
	int j = *b ;
	if (i <= j) {
	  __VERIFIER_assert (i <= j) ;
	} else { __VERIFIER_assert (i > j) ; }
}

void test_le20(int * num) {
	int i = *num ;
	if (i <= 20) {
	  __VERIFIER_assert (i <= 20) ;
	}
}

void test_lt0(int * num) {
	int n = (*a) ;
	if (0 < n) __VERIFIER_assert (0 < n) ;
	else __VERIFIER_assert (0 >= n) ;
}

void main() {}
