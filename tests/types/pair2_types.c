struct pair {
  (int with self = 4) x;
  (int with self = 2) y;
};

void test_const1(struct pair+ p);

void test_const2(struct pair+ p);

void test_exist1(struct pair+ p);

void test_exist2(struct pair+ p, int+ a) ;

void test_exist3(struct pair+ p, int+ a, int+ b) ;
