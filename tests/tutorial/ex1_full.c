#include <stdlib.h>

struct message {
  struct message *next;
  char *buffer;
};

struct message_box {
  int length;
  struct message *first;
};

void zeros_buffer(struct message_box * box) {
    
    struct message * first = box->first;
    struct message * current = first;
    
    int length = box->length;
    
    do {
        for (int i = 0; i<length; i++) {
            current->buffer[i] = 0 ;
        }
        current = current->next;
    } while(current != first) ;
}

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
