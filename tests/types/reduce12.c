//#include "/home/julien/Repos53/codex/tests/creduce2/rbtree_augmented.h"
#define u32 int

struct rb_node {
	unsigned long  __rb_parent_color;
	struct rb_node *rb_right;
	struct rb_node *rb_left;
} __attribute__((aligned(sizeof(long))));
/* The alignment might seem pointless, but allegedly CRIS needs it */

struct rb_root {
	struct rb_node *rb_node;
};

void test(struct rb_root *root, u32 ) {
  struct rb_node *node = root->rb_node;
  while (node) {
    node = node->rb_left;
  }
}
