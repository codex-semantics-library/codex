type string = ∃ a : (int with self > 1) .struct {
  (int with self = a) length ;
  (char[a])+ content ;
}

void test_access(string+ str);
void zeros(string+ str);

void zeros2(string+ str);
void zeros3(string+ str);
void zeros4(string+ str);
void zeros5(string+ str);

void fill_test(string+ str);
void fill_test_wrong(string+ str);
