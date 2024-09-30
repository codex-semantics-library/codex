struct Clock {
  int hour;
  int minute ;
};

struct Radio {
  int frequency ;
};

struct ClockRadio {
  struct Clock clock;
  struct Radio radio;
};

void test(struct ClockRadio * cl, struct Clock * c, struct Radio * r);
