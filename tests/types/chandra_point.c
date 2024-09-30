typedef struct {
  int x, y ;
} Point ;

typedef struct {
  int x, y, color ;
} ColorPoint ;

typedef struct {
  int common ;
  Point * p ;
} PS;

typedef struct {
  int common ;
  ColorPoint * p ;
} CPS;

enum Color {RED , BLUE , GREEN};


void test1(Point * pt, ColorPoint * pcp) {
  //Point p = *pt;
  __VERIFIER_debug_int32 ((int) pcp) ;
  pcp = (ColorPoint *) pt ;
  __VERIFIER_debug_int32 ((int) pcp) ;
  pcp->x = 1 ;
  pcp->color = RED ;
}

void test2(PS * psp, CPS * cps, CPS * cpsp, Point * pt, Point * q, ColorPoint * cp) {
  psp = (PS *) cps ;
  cpsp = cps ;
  q = pt ;
  psp->p = q ;
  cp = cpsp->p ;
  cp->color = RED ;
}

void test3(Point * pt, Point ** pp, ColorPoint ** q) {
  pp =  (Point**) q ;
  *pp = pt ;
  (*q)->color = RED ;
}


void main() {}
