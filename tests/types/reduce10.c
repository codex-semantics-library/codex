struct rbnode {
  struct rbnode *left;
  struct rbnode *right;
} *root, *n;

struct rbtree {
  struct rbnode *root
}

main() {}

main_insert(struct rbtree *tree, struct rbnode *node) {
  root = tree->root;
  n = node->right;
  if (root->left > 0) {
    node->right = root->left ;
  }
  __VERIFIER_flush_cache () ;
}
