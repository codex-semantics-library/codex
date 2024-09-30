struct message (len) {
   struct message(len)+ next;
   char[len]+ buffer;
};

∃ len:integer with self > 0.
struct message_box {
  integer with self = len length;
  struct message(len)+ first;
};

void zeros_buffer(struct message_box+ box);
