type rbnode_ptr = ∃ p : struct test_node+. union {
 int with (self = p + 4) a ;
 int with self = 0 b;
} 

struct rb_node {
  int parent_color ;
  rbnode_ptr right ;
  rbnode_ptr left ;
};

struct test_node {
  int key ;
  struct rb_node rb ;
};

struct rb_root_cached {
  rbnode_ptr rb_root;
  rbnode_ptr rb_leftmost;
};

struct rb_root {
  rbnode_ptr rb_node;
};

void test(struct rb_root+ root, int key);
