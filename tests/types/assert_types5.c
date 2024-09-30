type int3 = int with self = 3
type inf2 = int with self < 2

void test_eq2(int3+ a, inf2+ b);
void test_diff(int3+ a, inf2+ b);

