struct rbnode {
  struct rbnode *left;
  struct rbnode *right;
} *root, *n;

struct rbtree {
  struct rbnode *root;
};

void main() {}

void test(struct rbtree *tree, struct rbnode *node) {
  root = tree->root;
  n = node->right;
  node->right = root->left ;
  if (root->left)
    n->left = node;
}
