type paireq = ∃ a:int. struct {
  (int with self = a) x;
  (int with self = a) y;
}

void test1(paireq+ p);

void test2(paireq+ p);

void test3();
