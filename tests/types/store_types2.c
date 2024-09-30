type foo1 = ∃ a : int . (int with self = a)
type foo2 = ∃ b : int . (int with self = b)

void test(foo1+ a, foo2+ b);
