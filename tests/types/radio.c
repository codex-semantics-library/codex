//#include "binsec-stubs.h"

typedef struct {
  int hour, minute ;
} Clock ;

typedef struct {
  int frequency ;
} Radio ;

typedef struct {
  Clock clock;
  Radio radio;
} ClockRadio ;


void test(ClockRadio * cl, Clock * c, Radio * r) {
  //ClockRadio cr = *cl ;
  
  c = &(cl->clock) ;
  r = (Radio*) c + 1 ;
  r->frequency = 91 ;
}

void main() {}
