type bad_expr = struct {
  int tag ;
  int with (self = 100) data ;
}+

void test(bad_expr e);
