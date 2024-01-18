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

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

#ifdef _MSC_VER
#include <intrin.h>
#endif

__attribute__((__always_inline__))
static unsigned int clz(unsigned int v){
  /* Note: on a 64 bit platform, GCC's _builtin_clz will perform a 32
     bit operation (even if the argument has type int). We have to use
     _builtin_clzll instead. */
#if __GNUC__
  #ifdef ARCH_SIXTYFOUR  
    return __builtin_clzll(v);
  #else
    return __builtin_clz(v)
  #endif
#endif
#ifdef _MSC_VER
    int res = 0;     
  #ifdef ARCH_SIXTYFOUR  
    _BitScanReverse64(&res,v);
  #else
    _BitScanReverse(&res,v);
  #endif
    return res;
#endif    
}

/**************** Log2 (with rounding to the floor). ****************/
__attribute__((__always_inline__))
static uintnat inline_caml_int_builtin_log2 (value i){
  /* log2(v) is normally 32-1-clz(v), but because of the tag we
     must substract one more. */
  return (8*sizeof(value) - 2 - clz(i));
}

CAMLprim uintnat caml_int_builtin_log2 (value i){
  return inline_caml_int_builtin_log2(i);
}

CAMLprim uintnat caml_int_builtin_log2_untagged (uintnat i){
  return (8*sizeof(value) - 1 - clz(i));    
}

CAMLprim value caml_int_builtin_log2_byte (value i){
  return Val_int(inline_caml_int_builtin_log2(i));
}


/**************** Highest bit ****************/
CAMLprim uintnat caml_int_builtin_highest_bit (value i){
  return (1 << inline_caml_int_builtin_log2(i));
}

CAMLprim uintnat caml_int_builtin_highest_bit_untagged (uintnat i){
  return (1 << caml_int_builtin_log2_untagged(i));
}

CAMLprim value caml_int_builtin_highest_bit_byte (value i){
  return Val_int(1 << inline_caml_int_builtin_log2(i));
}


/**************** Find first set ****************/

__attribute__((__always_inline__))
static unsigned int ffs(unsigned int v){
  /* Note: on a 64 bit platform, GCC's _builtin_ffs will perform a 32
     bit operation (even if the argument has type int). We have to use
     _builtin_ffsll instead. */
#if __GNUC__
  #ifdef ARCH_SIXTYFOUR  
    return __builtin_ffsll(v);
  #else
    return __builtin_ffs(v)
  #endif
#endif
#ifdef _MSC_VER
#error Not done. Maybe using BitScanReverse.
#endif    
}


CAMLprim uintnat caml_int_builtin_ffs_untagged (uintnat i){
  return ffs(i);
}

CAMLprim value caml_int_builtin_ffs_byte (value i){
  return Val_int(ffs(Int_val(i)));
}


/**************** Count trailing zeroes.  ****************/
__attribute__((__always_inline__))
static unsigned int ctz(unsigned int v){
#if __GNUC__
  #ifdef ARCH_SIXTYFOUR  
    return __builtin_ctzll(v);
  #else
    return __builtin_ctz(v)
  #endif
#endif
#ifdef _MSC_VER
    int res = 0;     
  #ifdef ARCH_SIXTYFOUR  
    _BitScanForward64(&res,v);
  #else
    _BitScanForward(&res,v);
  #endif
    return res;
#endif    
}


CAMLprim uintnat caml_int_builtin_ctz_untagged (uintnat i){
  return ctz(i);
}

CAMLprim value caml_int_builtin_ctz_byte (value i){
  return Val_int(ctz(Int_val(i)));
}

/**************** Popcount. ****************/
/**************** Count trailing zeroes.  ****************/
__attribute__((__always_inline__))
static unsigned int popcount(unsigned int v){
#if __GNUC__
  #ifdef ARCH_SIXTYFOUR  
    return __builtin_popcountll(v);
  #else
    return __builtin_popcount(v)
  #endif
#endif
#ifdef _MSC_VER
  TODO
#endif    
}


CAMLprim uintnat caml_int_builtin_popcount_untagged (uintnat i){
  return popcount(i);
}

CAMLprim value caml_int_builtin_popcount_byte (value i){
  return Val_int(popcount(Int_val(i)));
}


