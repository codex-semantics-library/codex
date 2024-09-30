type pos = int with self > 0

type vector = ∃ sz : pos. (struct {
  (int with self = sz) length ;
  (int with self = 47)[sz] content ;
}+)

void test(vector+ v);
