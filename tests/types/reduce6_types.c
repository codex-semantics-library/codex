struct jsw_avlnode {
  int balance;
  int+ data;
  struct jsw_avlnode*[2] link;
};

struct jsw_avltree {
  struct jsw_avlnode* root;
  int size;
};

struct jsw_avltrav {
  struct jsw_avltree* tree;
  struct jsw_avlnode* it;
  struct jsw_avlnode*[64] path;
  int top;
};

void test(struct jsw_avltree+ tree);
