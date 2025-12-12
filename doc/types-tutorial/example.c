struct message {
  struct message *next;
  char *buffer;
};

struct message_box {
  int length;
  struct message *first;
};

void zeros_buffer(struct message_box *box) {
  struct message *first = box->first;
  struct message *current = first;

  int length = box->length;

  do {
    for (int i = 0; i < length; i++) {
      current->buffer[i] = 0;
    }
    current = current->next;
  } while (current != first);
}
