type app3 = <a, b> (int with self = (a + b))
type app4 = <a, b> (int with self = b)

void test(app4(2,5)+,(app3(1, 1)+));
