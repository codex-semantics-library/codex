struct array_n20 {
  (int with self = 20) length ;
  (char[20])+ content ;
}

void test_access(struct array_n20 * str);
void zeros(struct array_n20 * str);

void fill_test(struct array_n20 * str);
void fill_test_wrong(struct array_n20 * str);
