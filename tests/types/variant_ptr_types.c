type nat = int with self >= 0

  type expr_ptr = ∃ a : (nat with (self < 3)). ∃ p : expr_ts(a) with ((self & 7) == 0) . (int with (self == (p | a)))

type expr_ts = <a> union {
    (struct { int with self < 1000 val ; int with self = 0 padding ; }) with (a == 1)  constant ;
    (struct { expr_ptr a ; expr_ptr b ; }) with (a != 1) sum ;
}+

void test(expr_ptr ptr);
