struct message {
  struct message *next;
  char *buffer;
};

struct message_box {
  integer with self >= 0 length;
  struct message *first;
};

void zeros_buffer(struct message_box * box);
