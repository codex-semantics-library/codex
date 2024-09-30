#include <stdlib.h>

struct pair { 
  int a ;
  int b ;
};

struct list {
  int data ;
  struct list * next ;
};

struct pair * alloc_pair() {
  struct pair * p = malloc(sizeof(struct pair));
  if (__VERIFIER_nondet_int()) {
    p->a = 3 ;
	p->b = 1 ;
  } else {
	p->a = 2 ;
	p->b = 5 ;
  }
  return p ;
}

struct list * alloc_list() {
  struct list * l = malloc(sizeof(struct list));
  l->data = 0 ;
  l->next = NULL ;
  if (__VERIFIER_nondet_int()) {
    l->data = 11 ;
    l->next = alloc_list() ;
  }
  return l ;
}

int test_pair(struct pair * p) {
  if (__VERIFIER_nondet_int()) {
    p->a = 3 ;
  } else {
	p->a = 2 ;
  }
  return p->a ;
}

void main() {}
