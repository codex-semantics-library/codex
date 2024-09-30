struct string20 {
  (int with self = 20) length ;
  (char[20])+ content ;
};

void test_access(struct string20+ str);
void zeros(struct string20+ str);

void fill_test(struct string20+ str);
void fill_test_wrong(struct string20+ str);
