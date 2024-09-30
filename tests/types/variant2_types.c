type nat = int with self >= 0

type Lisp_object_union = <a> union {
    Lisp_Cons+ with (a = 3) list ;
    int with (a != 3) other ;
}

type Lisp_Object = ∃ a : (nat with (self < 8)). ∃ p : Lisp_object_union(a) with ((self & 7) = 0) . (int with (self = (p | a)))

type Lisp_Cons = struct {
  Lisp_Object car;
  union {
    Lisp_Object cdr;
    Lisp_Cons? chain;
  } u;
}

void test(List_Object ptr);
