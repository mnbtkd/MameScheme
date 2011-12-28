/*--------------------------------------------------------------------
  write.c -  MameScheme ( Scheme Interpreter based on R5RS )
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

/* following codes quoted from SigScheme */
#define SCM_USE_R6RS_NAMED_CHARS 1
typedef int32_t                    scm_ichar_t;
typedef struct ScmSpecialCharInfo_ ScmSpecialCharInfo;
struct ScmSpecialCharInfo_ {
/*     scm_ichar_t code; */     /* character code as ASCII/Unicode */
    const char code;     /* character code as ASCII/Unicode */
    const char *esc_seq;  /* escape sequence as string */
    const char *lex_rep;  /* lexical representation as character object */
};

const ScmSpecialCharInfo scm_special_char_table[] = {
    /* printable characters */
    {'\"',   "\\\"",  "\""},         /* 34, R5RS */
    {'\\',   "\\\\",  "\\"},         /* 92, R5RS */
    {' ',    " ",     "space"},      /* 32, R5RS */
#if SCM_USE_R6RS_CHARS
    {'|',    "\\|",   "|"},
#endif

    /* control characters */
    {'\n',   "\\n",   "newline"},    /*  10, R5RS */
#if SCM_USE_R6RS_NAMED_CHARS
    {'\0',   "\\x00", "nul"},        /*   0 */
    {'\a',   "\\a",   "alarm"},      /*   7 */
    {'\b',   "\\b",   "backspace"},  /*   8 */
    {'\t',   "\\t",   "tab"},        /*   9 */
    {'\n',   "\\n",   "linefeed"},   /*  10 */
    {'\v',   "\\v",   "vtab"},       /*  11 */
    {'\f',   "\\f",   "page"},       /*  12 */
    {'\r',   "\\r",   "return"},     /*  13 */
    {0x1b,   "\\x1b", "esc"},        /*  27 */
    {0x7f,   "\\x7f", "delete"},     /* 127 */
#endif /* SCM_USE_R6RS_NAMED_CHARS */
    {0, NULL, NULL}
};


void write_char(FILE* f, SchObj obj);
void write_string(FILE* f, SchObj obj);
void write_pair(FILE* f, SchObj obj);
void write_vector(FILE* f, SchObj obj);
void write_rational(FILE* f, SchRational* r);

#define WRITE_FIXNUM(f,obj)   (fprintf(f,"%ld",FIX2INT(obj)))
/* #define WRITE_FLOAT(f,obj)    (gmp_fprintf(f,"%.Ff",SCH_FLOAT_OBJ(obj)->f)) */
/* #define WRITE_FLOAT(f,obj)    (fprintf(f,"%-16g",SCH_FLOAT_OBJ(obj)->d)) */
#define WRITE_FLOAT(f,obj)    (fprintf(f,"%.16g",SCH_FLOAT_OBJ(obj)->d))
#define WRITE_RATIONAL(f,obj) (write_rational(f,obj))
#define WRITE_BIGNUM(f,obj)   (fprintf(f,"%s",bignum2str(obj,10)))
#define WRITE_SYMBOL(f,obj)   (fprintf(f,SCH_ID2NAME(obj)))
#define WRITE_CHAR(f,obj)     (write_char(f,obj))
#define WRITE_STRING(f,obj)   (write_string(f,obj))
#define WRITE_SUBR(f,obj)     (fprintf(f,"#<subr>"))
#define WRITE_LAMBDA(f,obj)   (fprintf(f,"#<closure #f>"))
#define WRITE_NIL(f,obj)      (fprintf(f,"()"))
#define WRITE_FALSE(f,obj)    (fprintf(f,"#f"))
#define WRITE_TRUE(f,obj)     (fprintf(f,"#t"))
#define WRITE_UNDEF(f,obj)    (fprintf(f,"#<undef>"))
#define WRITE_PAIR(f,obj)     (write_pair(f,obj))
#define WRITE_VECTOR(f,obj)   (write_vector(f,obj))

void write_rational(FILE* f, SchRational* r)
{
    SchObj n = r->numerator;
    SchObj d = r->denominator;

    if ( FIXNUMP(n) && FIXNUMP(d) ) {
        fprintf(f,"%d/%d",FIX2INT(n),FIX2INT(d));
    } else if ( FIXNUMP(n) ) {
        fprintf(f,"%d/%s",FIX2INT(n),bignum2str(d,10));
    } else if ( FIXNUMP(d) ) {
        fprintf(f,"%s/%d",bignum2str(n,10),FIX2INT(d));
    } else {
        fprintf(f,"%s/%s",bignum2str(n,10),bignum2str(d,10));
    }
}

void display_char(SchPort* port, SchObj obj )
{
    fputc(SCH_CHAR_OBJ(obj)->ch,SCH_PORT_OBJ(port)->src.stream.fp);
}
void write_char( FILE* f, SchObj obj ) /* TODO fix it */
{
    const ScmSpecialCharInfo *info;
    unsigned int c = SCH_CHAR_OBJ(obj)->ch;
    unsigned char b;
    fputs("#\\",f);
    for ( info = scm_special_char_table ; info->esc_seq ; info++ ) {
        if (c == info->code) {
            fprintf(f, info->lex_rep);
            return;
        }
    }

    b = (c & 0xff000000U) >> 24;  if (b) { fputc(b,f); }
    b = (c & 0xff0000U)   >> 16;  if (b) { fputc(b,f); }
    b = (c & 0xff00U)     >>  8;  if (b) { fputc(b,f); }
    b = (c & 0xffU)            ;  if (b) { fputc(b,f); }
}

void write_string(FILE* f, SchObj obj) /* TODO fix it */
{
    const ScmSpecialCharInfo *info;
    char  c;
    char* str  = SCH_STRING_OBJ(obj)->buf;
    int   size = SCH_STRING_OBJ(obj)->size;
    fputc('"',f);
    for ( ; (size>0) && (c = *str) ; size--, str++ ) {

        for (info = scm_special_char_table; info->esc_seq; info++) {
            if (c == info->code) {
                fprintf(f, info->esc_seq);
                goto next_char;
            }
        }

        {
            unsigned int ch_size = get_char_size(c);
            size -= (ch_size-1);
            do {
                fputc(c,f);
            } while ( (--ch_size > 0) && (c = *++str) );
        }

    next_char:
        ;
    }
    fputc('"',f);
}

void write_pair( FILE* f, SchObj obj )
{
    SchObj car;
    fprintf(f,"(");

    if (!NULLP(obj)) {
        car = SCH_CAR(obj);
        sch_write(f,car);
        obj = SCH_CDR(obj);
    }
    while ( !NULLP(obj) ) {
        if ( PAIRP(obj) ) {
            fprintf(f, " ");
            car = SCH_CAR(obj);
            sch_write(f,car);
            obj = SCH_CDR(obj);
        } else {
            fprintf(f," . ");
            sch_write(f,obj);
            obj = SCH_NIL;
        }
    }
    fprintf(f,")");

}
void write_vector( FILE* f, SchObj obj )
{
    int i,size;
    size = SCH_VECTOR_OBJ(obj)->size;

    fprintf(f,"#(");
    if ( size > 0 ) sch_write(f,SCH_VECTOR_OBJ(obj)->vec[0]);
    for ( i=1 ; i < size ; ++i) {
        fprintf(f," ");
        sch_write(f,SCH_VECTOR_OBJ(obj)->vec[i]);
    }

    fprintf(f,")");
}

void sch_write( FILE* f, SchObj obj )
{
    int i;
    i = sch_type(obj);

    switch(i){
    case T_FIXNUM:        WRITE_FIXNUM(f,obj);   break;
    case T_FLOAT:         WRITE_FLOAT(f,obj);    break;
    case T_RATIONAL:      WRITE_RATIONAL(f,obj); break;
    case T_BIGNUM:        WRITE_BIGNUM(f,obj);   break;
    case T_SYMBOL:        WRITE_SYMBOL(f,obj);   break;
    case T_CHAR:          WRITE_CHAR(f,obj);     break;
    case T_STRING:        WRITE_STRING(f,obj);   break;
    case T_PAIR:          WRITE_PAIR(f,obj);     break;
    case T_VECTOR:        WRITE_VECTOR(f,obj);   break;
    case T_SUBR:          WRITE_SUBR(f,obj);     break;
    case T_LAMBDA:        WRITE_LAMBDA(f,obj);   break;
/*   case T_ENV:          write_env(f,obj);      break; */
    case T_MISC_NIL:      WRITE_NIL(f,obj);      break;
    case T_MISC_FALSE:    WRITE_FALSE(f,obj);    break;
    case T_MISC_TRUE:     WRITE_TRUE(f,obj);     break;
/*   case T_MISC_EOF:     write_(f,obj); break;*/
/*   case T_MISC_UNBOUND: write_(f,obj); break;*/
    case T_MISC_UNDEFINE: WRITE_UNDEF(f,obj);    break;
/*   case T_MISC_DOT:     write_(f,obj); break;*/
/*   case T_MISC_KOKKA:   write_(f,obj); break;*/
    default: EXCEPTION("internal error at [sch_print]");
    }
 
}

#define DISPLAY_CHAR(f,obj)    (fputc(SCH_CHAR_OBJ(obj)->ch,f))
#define DISPLAY_STRING(f,obj)  (fputs(SCH_STRING_OBJ(obj)->buf,f))

void sch_display( FILE* f, SchObj obj )
{
    int i;
    i = sch_type(obj);

    switch(i){
    case T_FIXNUM:        WRITE_FIXNUM(f,obj);   break;
    case T_FLOAT:         WRITE_FLOAT(f,obj);    break;
    case T_RATIONAL:      WRITE_RATIONAL(f,obj); break;
    case T_BIGNUM:        WRITE_BIGNUM(f,obj);   break;
    case T_SYMBOL:        WRITE_SYMBOL(f,obj);   break;
    case T_CHAR:          DISPLAY_CHAR(f,obj);   break;
    case T_STRING:        DISPLAY_STRING(f,obj); break;
    case T_PAIR:          WRITE_PAIR(f,obj);     break;
    case T_VECTOR:        WRITE_VECTOR(f,obj);   break;
    case T_SUBR:          WRITE_SUBR(f,obj);     break;
    case T_LAMBDA:        WRITE_LAMBDA(f,obj);   break;
/*   case T_ENV:          write_env(f,obj);      break; */
    case T_MISC_NIL:      WRITE_NIL(f,obj);      break;
    case T_MISC_FALSE:    WRITE_FALSE(f,obj);    break;
    case T_MISC_TRUE:     WRITE_TRUE(f,obj);     break;
/*   case T_MISC_EOF:     write_(f,obj); break;*/
/*   case T_MISC_UNBOUND: write_(f,obj); break;*/
    case T_MISC_UNDEFINE: WRITE_UNDEF(f,obj);    break;
/*   case T_MISC_DOT:     write_(f,obj); break;*/
/*   case T_MISC_KOKKA:   write_(f,obj); break;*/
    default: EXCEPTION("internal error at [sch_print]");
    }
}

#define END_OF_LINE "\r\n"

void sch_newline( SchPort* port )
{
    char* c;
    for ( c = END_OF_LINE; *c ; c++ ) {
        fputc(*c,port->src.stream.fp);
    }
}

