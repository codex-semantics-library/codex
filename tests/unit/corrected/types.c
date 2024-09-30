type rbnode = struct {
  rbnode? rbe_left;
  rbnode+ rbe_right;
  rbnode+ rbe_parent;
  int rbe_color;
  int i;
}

type rbtree = struct {
  rbnode+ rbh_root;
}
