// This could trigger an infinite loop in codex because the initial loop step
// was refined during the loop:
//
// The access to f->elt tells us that the numeric value of f must be smaller
// than MAX_INT - offset_of(elt),.

struct intlist {
  struct intlist *next;
  int elt;
};

void main(struct intlist *f) {
  int x;
  while (f->elt) {
    f = f->next;
    // x = f->elt;
  }
}
