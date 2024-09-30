struct string15 {
  (int with self = 15) length ;
  (char[20])+ content ;
};

void test_access(struct string15+ str);
void zeros(struct string15+ str);

void fill_test(struct string15+ str);
void fill_test_wrong(struct string15+ str);
