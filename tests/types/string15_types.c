struct array_n15 {
  (int with self = 15) length ;
  (char[20])+ content ;
}

void test_access(struct array_n15 * str);
void zeros(struct array_n15 * str);

void fill_test(struct array_n15 * str);
void fill_test_wrong(struct array_n15 * str);
