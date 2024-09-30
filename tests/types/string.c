//

struct string {
  int length ;
  char * content ;
} ;


void test_access(struct string * str) {
	str->content[0] = '0' ;
	__VERIFIER_assert(str->content[0] != '7') ;
	//__VERIFIER_flush_cache() ;
}


void zeros(struct string * str) {
	char * ptr = str->content ;
	while (ptr < str->content + str->length) {
		*ptr = '0' ;
		ptr = ptr + 1 ;
	}
	//__VERIFIER_flush_cache() ;
}

// should only work if we use the induction variable analysis domain
void zeros2(struct string * str) {
	int * init = str->content ;
	//int * ptr = init ;
	int length = str->length ;
	for (int i = 0; i < length; i++) {
		*(init + i) = '0' ;
	}
	//__VERIFIER_flush_cache() ;
}

// should only work if we use the induction variable analysis domain
void zeros3(struct string * str) {
	int * init = str->content ;
	int * ptr = init ;
	int length = str->length ;
	int * end = init + length ;
	for (int i = length; i > 0; i--) {
		__VERIFIER_assert ((end - i) == ptr) ;
		*ptr = 0 ;
		ptr++ ;
	}
}

void zeros4(struct string * str) {
	char * ptr = str->content ;
	char * init = ptr ;
	int length = str->length ;
	for (int i = 0; i < length; i++) {
		__VERIFIER_assert ((init + i) == ptr) ;
		ptr = '0' ;
		ptr++ ;
	}
}

void zeros5(struct string * str) {
	char * ptr = str->content ;
	int i = 0 ;
	while (i < str->length) {
		ptr[i] = '0' ;
		i++ ;
	}
	//__VERIFIER_flush_cache() ;
}

void fill_test(struct string * str) {
	char * ptr = str->content ;
	//int i = 0 ;
	//while (i < str->length) {
		//ptr[i] = 'k' ;
		//i++ ;
	//}
	char * last = str->content + str->length ;
	//if ((0 < ptr) && (ptr < str->content + str->length)) {
	while (ptr < last) {
		*ptr = 'k' ;
		ptr++ ;
	}
	//__VERIFIER_flush_cache() ;
}

void fill_test_wrong(struct string * str) {
	int nb = str->length ;
	char * ptr = str->content ;
	int i = 0 ;
	while ((i < 0) || (i == 0) && (i < nb)) {
		ptr[i] = 'k' ;
		i++ ;
	}
	//__VERIFIER_flush_cache() ;
}

void main() {}
