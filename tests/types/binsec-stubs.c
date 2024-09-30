#include "binsec-stubs.h"

void *malloc_uf_node() {
  return (void *)(-1);
}

void *malloc_T() {
  return (void *)(-1);
}

void *malloc_elist(__attribute__((unused)) size_t length) {
  return (void *)(-1);
}

int *malloc_int() {
  return (void *)(-1);
}

void *malloc__gdsl_node() {
  return (void *)(-1);
}

void *malloc__gdsl_list() {
  return (void *)(-1);
}

void *malloc__gdsl_list_cursor() {
  return (void *)(-1);
}

void *malloc__gdsl_bintree() {
  return (void *)(-1);
}

void *malloc_gdsl_bstree() {
  return (void *)(-1);
}

void *malloc_jsw_avlnode() {
  return (void *)(-1);
}

void *malloc_jsw_avltree() {
  return (void *)(-1);
}

void *malloc_jsw_avltrav() {
  return (void *)(-1);
}

void *malloc_bstree() {
  return (void *)(-1);
}

void *malloc_rbnode() {
  return (void *)(-1);
}

void *malloc_rbtree() {
  return (void *)(-1);
}

void *malloc_snode() {
  return (void *)(-1);
}

void *malloc_stree() {
  return (void *)(-1);
}

void *malloc_edge() {
  return (void *)(-1);
}

void *malloc_node() {
  return (void *)(-1);
}

int __VERIFIER_nondet_int() {
  return 1;
}

void free(__attribute__((unused))  void * p) {
}

void assert(__attribute__((unused)) bool c) {
}

void use(__attribute__((unused)) void *p) {
}

void _memcad(__attribute__((unused)) char *s) {
}

/** Extensions */

void __VERIFIER_assert(__attribute__((unused)) bool c) {
}

void __VERIFIER_flush_cache() {
  return (void *)(-1);
}
