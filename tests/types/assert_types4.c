type int1 = int with self = 1
type inf2 = int with self < 2

void test_eq2(int1+ a, inf2+ b);
void test_diff(int1+ a, inf2+ b);

