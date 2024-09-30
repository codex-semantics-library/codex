struct rbnode {
  struct rbnode* rbe_left;
  struct rbnode+ rbe_right;
  struct rbnode+ rbe_parent;
  int rbe_color;
  int i;
};

struct rbtree {
  struct rbnode+ rbh_root;
};

void test(struct rbtree+ a, struct rbnode+ b);
