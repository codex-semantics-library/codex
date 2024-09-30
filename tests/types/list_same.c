struct alist {
  int data;
  struct alist* next;
};

void list_iter(struct alist * lst, int * a) {
  while (lst != 0) {
    *a = lst->data ;
    lst = lst->next ;
  }
}

void list_iter_check(struct alist * lst, int * a) {
  while (lst != 0) {
    __VERIFIER_assert(*a == lst->data) ;
    *a = lst->data ;
    lst = lst->next ;
  }
}

void main() {}
