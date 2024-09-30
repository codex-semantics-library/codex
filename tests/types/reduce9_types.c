struct rbnode {
  struct rbnode* rbe_left;
  struct rbnode+ rbe_right;
  struct rbnode+ rbe_parent;
  int rbe_color;
  int i;
}

struct rbtree {
  struct rbnode+ rbh_root;
}

void main_insert(struct rbtree+, struct rbnode+);
