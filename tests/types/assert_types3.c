type int1 = int with self = 1
type int3 = int with self = 3

void test_eq2(int1+ a, int3+ b);
void test_diff(int1+ a, int3+ b);

