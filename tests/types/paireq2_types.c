type paireq = ∃ a:int. struct {
  (int with self = a) x;
  (int with self = a) y;
}

void test_const1(paireq+ p);

void test_const2(paireq+ p);

void test_exist1(paireq+ p);

void test_exist2(paireq+ p, int+ a) ;

void test_exist3(paireq+ p, int+ a, int+ b) ;
