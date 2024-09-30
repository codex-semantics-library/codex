struct Point {
  int x;
  int y ;
};

struct ColorPoint {
  int x;
  int y;
  int color ;
};

struct PS {
  int common ;
  struct Point+ p ;
}

struct CPS {
  int common ;
  struct ColorPoint+ p ;
}

void test1(struct Point * pt, struct ColorPoint * pcp);

void test2(struct PS * psp, struct CPS * cps, struct CPS * cpsp, struct Point * pt, struct Point * q, struct ColorPoint * cp);

void test3(struct Point * pt, struct Point ** pp, struct ColorPoint ** q);

