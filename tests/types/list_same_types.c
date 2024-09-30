struct alist {
  int data;
  struct alist* next;
};

void list_iter(struct alist * lst, int * a);
void list_iter_check(struct alist * lst, int * a);
