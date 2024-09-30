type int1 = int with self = 1

type app1 = <a> int with self = a

void test(app1(1)+, int1+);
