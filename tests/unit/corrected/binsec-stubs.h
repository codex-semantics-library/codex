#ifndef BINSEC_STUBS_H
#define BINSEC_STUBS_H

#include <stdio.h> /* For FILE and fprintf */

#ifndef NULL
#define NULL 0
#endif
typedef unsigned int size_t;
typedef enum {
  false = 0,
  true = 1
} bool;

int __VERIFIER_nondet_int(void);

int *malloc_int(void);
void *malloc_uf_node(void);
void *malloc_T(void);
void *malloc_elist(size_t);
void *malloc_bstree(void);
void *malloc__gdsl_node(void);
void *malloc__gdsl_list(void);
void *malloc__gdsl_list_cursor(void);
void *malloc__gdsl_bintree(void);
void *malloc_gdsl_bstree(void);
void *malloc_jsw_avlnode(void);
void *malloc_jsw_avltree(void);
void *malloc_jsw_avltrav(void);
void *malloc_rbnode(void);
void *malloc_rbtree(void);
void *malloc_snode(void);
void *malloc_stree(void);
void *malloc_edge(void);
void *malloc_node(void);
void free(void *);

void assert(bool);

/* To prevent the compiler from foregoing unused variables. */
void use(void *p);

/* We redeclare malloc with an incompatible type to avoid using it by accident
 * in the source */
void malloc(void);

/* To analyze MemCAD benchmarks without removing all _memcad directives. This
 * does nothing. */
void _memcad(char*);

/** Extensions */

void __VERIFIER_assert(bool);

void __VERIFIER_flush_cache(void);

void __VERIFIER_debug_int32(int);

#endif /* BINSEC_STUBS_H */

