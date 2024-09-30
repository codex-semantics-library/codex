struct foo {
  int p[5];
};

void test(struct foo * a){
  int * ptr = &(a->p) ;
  ptr += 3 ;
  ptr += (-1) ;
  ptr = ptr ;
}

void main(){}
