/**************************************************************************/
/*  This file is part of the Codex semantics library.                     */
/*                                                                        */
/*  Copyright (C) 2013-2025                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file LICENSE).                      */
/*                                                                        */
/**************************************************************************/

struct message {
  struct message *next;
  char* buffer;
};


struct message_box {
  int size;
  struct message* to_read;
  struct message* free_list;
};

struct task{
  struct message_box* box;
  int lines_seen;
};


int count_newlines(char *buffer, int len){
  int count = 0;
  for(int i = 0; i < len; i++){
    if(buffer[i] == '\n') count++;
  }
  return count;
}

void main(struct task* task){
  
  struct message_box* box = task->box;
  int size = box->size;
  struct message* ptr = box->to_read;
  while(ptr){
    /* Count the lines. */
    int res = count_newlines(&(ptr->buffer[0]),size);
    task->lines_seen += res;

    /* Move the message to the free list. */
    struct message* next = ptr->next;
    ptr->next = box->free_list;
    box->free_list = ptr;
    ptr = next;
  }
}
