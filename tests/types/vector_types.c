type vector = ∃ a : (int with self > 2) . struct {
  (int with self = a) length ;
  char[a]+ content ;
}

void test(vector+ v);
