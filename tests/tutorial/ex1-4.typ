type struct message = 
∃ len:integer with self >= 0. (
struct {
   struct message+ next;
   char[len]+ buffer;
}) 

struct message_box { 
  integer with self >= 0 length;
  struct message+ first;
};

void zeros_buffer(struct message_box+ box);
