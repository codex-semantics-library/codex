type expr = ∃ a : (nat with self < 2) . (struct {
  int with (self = a) tag ;
  union {
    (int with (self = 100)) with (a = 0) a ;
    (int with (self = 200)) with (a != 0) b ;
  } data ;
})+

void test(expr e);

