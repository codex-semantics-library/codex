type foo = int

type bar = foo with self = 3

void test(bar++ a, foo+ b);

void test2(bar+ a);
