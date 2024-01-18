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

#include <stdio.h>

#include <sys/time.h>
#include <sys/resource.h>

static struct timeval libase_recorded_time;

void libase_record_time(void){
  struct timezone tzp;
  gettimeofday(&libase_recorded_time, &tzp);
};

int libase_return_time(void){
  struct timeval t;
  struct timezone tzp;
  gettimeofday(&t, &tzp);
  return (t.tv_sec - libase_recorded_time.tv_sec) * 1000000 + (t.tv_usec - libase_recorded_time.tv_usec);
}

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

CAMLprim value caml_libase_return_time(void){
  return Val_int(libase_return_time());
}
