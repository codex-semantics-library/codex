#include "binsec-stubs.h"

type int1 = int with self = 1
type foo1 = ∃ a . (int with self = a)

void test(int1 * a, foo1 * b);

