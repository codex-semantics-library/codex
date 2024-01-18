/**************************************************************************/
/*  This file is part of the Codex semantics library.                     */
/*                                                                        */
/*  Copyright (C) 2013-2024                                               */
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

#include <termios.h>
#include <unistd.h>
#include <stdlib.h>

int rand_int(void);
unsigned char rand_char(void);

int tcsetattr(int fd, int optional_actions,
              struct termios *termios_p){
  unsigned char *ptr;
  for(ptr=termios_p;ptr < termios_p + 1; ptr++)
    *ptr = rand_char();
  return rand_int();
}

int tcgetattr(int ft, struct termios *termios_p){
  unsigned char *ptr;
  for(ptr=termios_p;ptr < termios_p + 1; ptr++)
    *ptr = rand_char();
  return rand_int();
}

int snprintf(char *str, size_t size, const char *format, ...){
  unsigned char *ptr;
  for(ptr = str; ptr < str + size; ptr++) {
    *ptr = rand_char();
  };

  int v = rand_int();
  if(0 <= v && v < size) return v;
  else while(1);
}

void *memcpy(void *dst, void *source, size_t n){
  int i;
  char *dest = dst;
  char *src = source;
  for(i = 0; i < n; i++) dest[i] = src[i];
  return dest;
}

void *memset(void *dst, int c, size_t n){
  int i;
  char *dest = dst;
  for(i = 0; i < n; i ++) dest[i] = c;
  return dest;
}
