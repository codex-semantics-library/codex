struct message {
  struct message *next;
  char *buffer;
};

struct message_box {
  int length;
  struct message *first;
};

void zeros_buffer(struct message_box + box);
void main();
