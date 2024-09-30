#include "reduce6.h"

void test(struct jsw_avltree *a, int *data) {
  if (a->root == NULL)
    cmp_f(a->root->data, data);
}

void main() {}
