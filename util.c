/*--------------------------------------------------------------------
  util.c -  Mame ( Scheme Interpreter based on R5RS )
  --------------------------------------------------------------------*/
/*
 * Copyright (c) 2007-2011  mnbtkd
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of authors nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include "mame.h"

char* type_char(SchObj obj)
{
  int i;
  i = sch_type(obj);
  switch(i){
  case T_FIXNUM:       return "T_FIXNUM";
  case T_BIGNUM:       return "T_BIGNUM";
  case T_FLOAT:        return "T_FLOAT";
  case T_SYMBOL:       return "T_SYMBOL";
  case T_CHAR:         return "T_CHAR";
  case T_STRING:       return "T_STRING";
  case T_PAIR:         return "T_PAIR";
  case T_VECTOR:       return "T_VECTOR";
  case T_SUBR:         return "T_SUBR";
  case T_LAMBDA:       return "T_LAMBDA";
  case T_ENV:          return "T_ENV";
  case T_MISC_NIL:     return "T_MISC_NIL";
  case T_MISC_FALSE:   return "T_MISC_FALSE";
  case T_MISC_TRUE:    return "T_MISC_TRUE";
  case T_MISC_EOF:     return "T_MISC_EOF";
  case T_MISC_UNBOUND: return "T_MISC_UNBOUND";
  case T_MISC_UNDEFINE:return "T_MISC_UNDEFINE";
  case T_MISC_DOT:     return "T_MISC_DOT";
  case T_MISC_KOKKA:   return "T_MISC_KOKKA";
  default: return "error : unknown object";
  }
}

SchObj car(SchObj obj)
{
    return ((SchPair*)obj)->car;
}

SchObj cdr( SchObj obj )
{
    return ((SchPair*)obj)->cdr;
}
