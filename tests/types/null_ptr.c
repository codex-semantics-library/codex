struct pair {
  int i;
  int j;
};


void test(struct pair* p){
  int a = p->i + p -> j;
  a = a+1;
  __VERIFIER_assert(((p->i + p->j) + 1) == a);
}

void main(){}
