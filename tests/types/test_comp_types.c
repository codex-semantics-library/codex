type int1 = int with self = 1
type int2 = int with self = 2
type int3 = int with self = 3

type app1 = <a> int with self = a
type app2 = <b> int with self = b
type app3 = <a, b> (int with self = (a + b))
type app4 = <a, b> (int with self = b)

type alist = <a> struct {
  (int with self = a) data ;
  alist(a)? next ;
}

type list5 = struct {
  (int with self = 5) data ;
  alist? next ;
}

type foo1 = ∃ a . (int with self = a)
type foo2 = ∃ b . (int with self = b)

type pair =  struct {
  int x ;
  int y ; 
}

type pairEq = ∃ a . struct {
  (int with self = a) x ;
  (int with self = a) y ; 
}

type pairOfPair = struct {
  pairEq+ u ;
  pairEq+ v ;
}

type pair13_25 = struct {
  (int with self = 13) x ;
  (int with self = 25) y ;

}

type pairEq_ptr = ∃ a . (struct {
  (int with self = a) x ;
  (int with self = a) y ;
}+)

type pair13_25_ptr = (struct {
  (int with self = 13) x ;
  (int with self = 25) y ;

})+


type array_n20 = struct {
  (int with self = 20) length ;
  (char[20])+ content ;
}

type array_n15 = struct {
  (int with self = 15) length ;
  (char[20])+ content ;
}

type int_pos = int with self > 1

type string = ∃ a . struct {
  (int_pos with self = a) length ;
  (char[a])+ content ;
}
