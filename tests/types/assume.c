typedef struct foo {
  int tag ;
  int data ;
} foo ;

void test(struct foo * ptr) {
	//__VERIFIER_assert (ptr->data) ;
	if (ptr->tag == 0) {
	  __VERIFIER_assert(ptr->data == 100) ;
	}
	else {
	  __VERIFIER_assert(ptr->data == 200) ;
	}
	//__VERIFIER_flush_cache () ;
}

int main(void) {
	return 0;
}
