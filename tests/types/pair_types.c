type pair_eq = ∃ a : int . struct {
  (int with self = a) x;
  (int with self = a) y;
}

type pair_of_pair = struct {
  pair_eq+ p1;
  pair_eq+ p2;
}

void test_pair_eq(pair_eq+ a);
void test_pair_of_pair_eq(pair_of_pair+ a);
void test_pair_of_pair_eq2(pair_of_pair+ a);
void test_pair_of_pair_eq3(pair_eq+ a, pair_eq+ b);
void write_1_x_4(pair_eq+ a);

