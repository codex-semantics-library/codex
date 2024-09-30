type nat = int with self >= 0

type expr =  ∃ a : (nat with self < 2) . (expr_t(a)+)

type expr_t = <a> struct {
  int with (self = a) tag ;
  union {
    (struct { int with self < 1000 val ; int with self = 0 padding ; }) with (a = 0)  constant ;
    (struct { expr a ; expr b ; }) with (a != 0) sum ;
  } data ;
}

void test(expr e);

