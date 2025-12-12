#include <stddef.h>
#include <stdlib.h>
#include "example.c"

int main(void) {
  // Allocates the message box
  struct message_box *box = malloc(sizeof(struct message_box));
  box->length = 20;
  box->first = NULL;
  for (int i = 0; i < 10; i++) {
    struct message *lst = malloc(sizeof(struct message));
    lst->buffer = malloc(sizeof(char) * box->length);
    lst->next = box->first;
    box->first = lst;
  }

  // Fills the content of message box with zeros
  zeros_buffer(box);

  return 0;
}
