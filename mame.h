/*--------------------------------------------------------------------
  mame.h -  MameScheme ( Scheme Interpreter based on R5RS )
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

#ifndef INCLUDE_MAME_H
#define INCLUDE_MAME_H

#define DEBUG

/* #include "config.h" */

#include <string.h>
#include <stdio.h>
#include <limits.h>

/* #include <gmp.h> */

/* #define ENCODING_SJIS */
#define ENCODING_UTF8

/* BOOLEAN */
typedef int BOOL;
#define TRUE  1
#define FALSE 0

/* Scheme Objects */
#define T_HEAPOBJ       0x00 /* -------- -------- -------- ------00 */
#define T_FIXNUM        0x01 /* -------- -------- -------- ------01 */
#define T_MISC          0x02 /* -------- -------- -------- ------10     NIL #f #t UNDEFINE EOF UNBOUND ... */
#define T_SYMBOL        0x03 /* -------- -------- -------- ------11     max 1073741823 symbols */
/* heap objects */
#define T_CHAR          0x04
#define T_STRING        0x05
#define T_PAIR          0x06
#define T_VECTOR        0x07
#define T_SUBR          0x08
#define T_LAMBDA        0x09
#define T_ENV           0x0a
#define T_PORT          0x0b
#define T_BOX           0x0c
#define T_FLOAT         0x0d
#define T_RATIONAL      0x0e
#define T_BIGNUM        0x0f

#define T_MASK          0x03 /* -------- -------- -------- ----0011 */
#define T_HEAPMASK      0x0f /* -------- -------- -------- ----1111 */

/* misc types */
#define T_MISC_NIL      0xf0
#define T_MISC_FALSE    0xf1
#define T_MISC_TRUE     0xf2
#define T_MISC_EOF      0xf3
#define T_MISC_UNBOUND  0xf4
#define T_MISC_UNDEFINE 0xf5
#define T_MISC_DOT      0xf6
#define T_MISC_KOKKA    0xf7

#define MAKE_MISC_OBJ(i) (((i)<<2) + T_MISC)
#define SCH_NIL      ((SchObj)MAKE_MISC_OBJ(0)) /* -------- -------- -------- ---00010 */
#define SCH_FALSE    ((SchObj)MAKE_MISC_OBJ(1)) /* -------- -------- -------- ---00110 */
#define SCH_TRUE     ((SchObj)MAKE_MISC_OBJ(2)) /* -------- -------- -------- ---01010 */
#define SCH_EOF      ((SchObj)MAKE_MISC_OBJ(3)) /* -------- -------- -------- ---01110 */
#define SCH_UNBOUND  ((SchObj)MAKE_MISC_OBJ(4)) /* -------- -------- -------- ---10010 */
#define SCH_UNDEFINE ((SchObj)MAKE_MISC_OBJ(5)) /* -------- -------- -------- ---10110 */
#define SCH_DOT      ((SchObj)MAKE_MISC_OBJ(6)) /* -------- -------- -------- ---11010 */
#define SCH_KOKKA    ((SchObj)MAKE_MISC_OBJ(7)) /* -------- -------- -------- ---11110 */


#define SCH_TAG(obj)   ((obj) & T_MASK)

#define HEAPOBJP(obj)  (SCH_TAG(obj) == T_HEAPOBJ)
#define FIXNUMP(obj)   (SCH_TAG(obj) == T_FIXNUM)
#define MISCP(obj)     (SCH_TAG(obj) == T_MISC)
#define SYMBOLP(obj)   (SCH_TAG(obj) == T_SYMBOL)

#define SCH_TYPE(obj)  sch_type(obj)

#define STRINGP(obj)   (SCH_TYPE(obj) == T_STRING)
#define CHARP(obj)     (SCH_TYPE(obj) == T_CHAR)
#define PAIRP(obj)     (SCH_TYPE(obj) == T_PAIR)
#define ENVP(obj)      (SCH_TYPE(obj) == T_ENV)
#define FALSEP(obj)    (SCH_TYPE(obj) == T_MISC_FALSE)
#define TRUEP(obj)     (SCH_TYPE(obj) == T_MISC_TRUE)
#define NULLP(obj)     (SCH_TYPE(obj) == T_MISC_NIL)
#define EOFP(obj)      (SCH_TYPE(obj) == T_MISC_EOF)
#define UNBOUNDP(obj)  (SCH_TYPE(obj) == T_MISC_UNBOUND)
#define SUBRP(obj)     (SCH_TYPE(obj) == T_SUBR)
#define LAMBDAP(obj)   (SCH_TYPE(obj) == T_LAMBDA)
#define VECTORP(obj)   (SCH_TYPE(obj) == T_VECTOR)
#define BOOLEANP(obj)  (FALSEP(obj)||TRUEP(obj))
#define PORTP(obj)     (SCH_TYPE(obj) == T_PORT)
#define BOXP(obj)      (SCH_TYPE(obj) == T_BOX)
#define FLOATP(obj)    (SCH_TYPE(obj) == T_FLOAT)
#define RATIONALP(obj) (SCH_TYPE(obj) == T_RATIONAL)
#define BIGNUMP(obj)   (SCH_TYPE(obj) == T_BIGNUM)
#define UNDEFINEP(obj) (SCH_TYPE(obj) == T_MISC_UNDEFINE)

#define INTP(obj)      (FIXNUMP(obj)||BIGNUMP(obj))

/* #define MISC_TYPE(obj) misc_type(obj) */

static inline int misc_type(obj)
{
    return ((obj >> 2) | 0xf0);
}

typedef struct SchHeaderRec {
    unsigned long flags;
} SchHeader;

#define SCH_HEADER(obj) ((SchHeader*)obj)

static inline int sch_type(obj)
{
    if ( FIXNUMP(obj) ) return T_FIXNUM;
    if ( SYMBOLP(obj) ) return T_SYMBOL;
    if ( MISCP(obj)   ) return misc_type(obj);

    return (SCH_HEADER(obj)->flags & T_HEAPMASK);
}


typedef unsigned long SchObj;



/* FIXNUM */
#define INT2FIX(i)    ((SchObj)(((long)(i))<<3 | T_FIXNUM)) /* useful range 29bit (including sign/unsign bit)  MIN : -268435456   MAX : 268435455 */
#define FIX2INT(obj)  (((long)obj)>>3)
#define ZEROP(obj)    (FIX2INT(obj)==0)
#define EXACT_FLAG    0x04
#define FIXABLE(d)    ((d <= (INT_MAX>>3)) && ((INT_MIN>>3) <= d))

/* BIGNUM */
typedef struct SchBignumRec {
    SchHeader hd;
    char      sign;
    int       len;
    int       bytesize;
    void*     digits;
} SchBignum;
SchObj str2bignum(char* str, int radix);
char* bignum2str(SchBignum* obj, int base);
SchObj add_int( SchObj x, SchObj y );
SchObj sub_int( SchObj x, SchObj y );
SchObj mul_int( SchObj x, SchObj y );
#define SCH_BIGNUM_OBJ(obj)  ((SchBignum*)obj)


/* FLOAT */
/* #define D_PREC 20 */
typedef struct SchFloatRec {
    SchHeader hd;
    double d;                   /* mpf_t f; */
} SchFloat;
SchObj make_float(double d);    /* SchObj make_float(mpf_t f); */
#define SCH_FLOAT(d)       make_float(d)
#define SCH_FLOAT_OBJ(obj) ((SchFloat*)obj)

/* RATIONAL */
typedef struct SchRationalRec {
    SchHeader hd;
    SchObj numerator;
    SchObj denominator;
} SchRational;
SchObj rational(SchObj n, SchObj d);
#define SCH_RATIONAL(n,d)     (rational(n,d))
#define SCH_RATIONAL_OBJ(obj) ((SchRational*)obj)

/* PAIR */
typedef struct SchPairRec {
    SchHeader hd;
    SchObj car;
    SchObj cdr;
} SchPair;

#define SCH_PAIR(obj)          ((SchPair*)obj)
#define SCH_CAR(obj)           (SCH_PAIR(obj)->car)
#define SCH_CDR(obj)           (SCH_PAIR(obj)->cdr)
#define SCH_SET_CAR(obj,val)   (SCH_CAR(obj) = val)
#define SCH_SET_CDR(obj,val)   (SCH_CDR(obj) = val)
#define SCH_CONS(car,cdr)      (make_pair(car,cdr))
int list_length( SchObj list );
#define SCH_LENGTH(lst)        (list_length(lst))
int is_list(SchObj list);
#define LISTP(obj) is_list(obj)

#define SCH_LIST1(a)         (SCH_CONS(a,SCH_NIL))
#define SCH_LIST2(a,b)       (SCH_CONS(a,SCH_LIST1(b)))
#define SCH_LIST3(a,b,c)     (SCH_CONS(a,SCH_LIST2(b,c)))
#define SCH_LIST4(a,b,c,d)   (SCH_CONS(a,SCH_LIST3(b,c,d)))
#define SCH_LIST5(a,b,c,d,e) (SCH_CONS(a,SCH_LIST4(b,c,d,e)))
#define SCH_LIST6(a,b,c,d,e,f)       (SCH_CONS(a,SCH_LIST5(b,c,d,e,f)))
#define SCH_LIST7(a,b,c,d,e,f,g)     (SCH_CONS(a,SCH_LIST6(b,c,d,e,f,g)))
#define SCH_LIST8(a,b,c,d,e,f,g,h)   (SCH_CONS(a,SCH_LIST7(b,c,d,e,f,g,h)))
#define SCH_LIST9(a,b,c,d,e,f,g,h,i) (SCH_CONS(a,SCH_LIST8(b,c,d,e,f,g,h,i)))
#define SCH_LIST10(a,b,c,d,e,f,g,h,i,j)             (SCH_CONS(a,SCH_LIST9( b,c,d,e,f,g,h,i,j)))
#define SCH_LIST11(a,b,c,d,e,f,g,h,i,j,k)           (SCH_CONS(a,SCH_LIST10(b,c,d,e,f,g,h,i,j,k)))
#define SCH_LIST12(a,b,c,d,e,f,g,h,i,j,k,l)         (SCH_CONS(a,SCH_LIST11(b,c,d,e,f,g,h,i,j,k,l)))
#define SCH_LIST13(a,b,c,d,e,f,g,h,i,j,k,l,m)       (SCH_CONS(a,SCH_LIST12(b,c,d,e,f,g,h,i,j,k,l,m)))
#define SCH_LIST14(a,b,c,d,e,f,g,h,i,j,k,l,m,n)     (SCH_CONS(a,SCH_LIST13(b,c,d,e,f,g,h,i,j,k,l,m,n)))
#define SCH_LIST15(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)   (SCH_CONS(a,SCH_LIST14(b,c,d,e,f,g,h,i,j,k,l,m,n,o)))
#define SCH_LIST16(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) (SCH_CONS(a,SCH_LIST15(b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)))

/* #define USE_BOEHM_GC */

#include <stdlib.h>
#ifdef USE_BOEHM_GC
  #include <gc.h>
  #define SCH_MALLOC(__size__)     GC_malloc(__size__)
  #define SCH_MALLOC_ATM(__size__) GC_malloc_atomic(__size__)
  #define SCH_CALLOC(__size__,__n__)     GC_malloc((__size__)*(__n__))
  #define SCH_REALLOC    GC_realloc
  #define SCH_FREE(__ptr__)
#else
  #define SCH_MALLOC(__size__)     malloc(__size__)
  #define SCH_MALLOC_ATM(__size__) malloc(__size__)
  #define SCH_CALLOC     calloc
  #define SCH_REALLOC    realloc
  #define SCH_FREE(__ptr__)        free(__ptr__)
#endif

#define SCH_CAAR(obj)    (SCH_CAR(SCH_CAR(obj)))
#define SCH_CADR(obj)    (SCH_CAR(SCH_CDR(obj)))
#define SCH_CDAR(obj)    (SCH_CDR(SCH_CAR(obj)))
#define SCH_CDDR(obj)    (SCH_CDR(SCH_CDR(obj)))

#define SCH_CAAAR(obj)   (SCH_CAR(SCH_CAR(SCH_CAR(obj))))
#define SCH_CAADR(obj)   (SCH_CAR(SCH_CAR(SCH_CDR(obj))))
#define SCH_CADAR(obj)   (SCH_CAR(SCH_CDR(SCH_CAR(obj))))
#define SCH_CADDR(obj)   (SCH_CAR(SCH_CDR(SCH_CDR(obj))))
#define SCH_CDAAR(obj)   (SCH_CDR(SCH_CAR(SCH_CAR(obj))))
#define SCH_CDADR(obj)   (SCH_CDR(SCH_CAR(SCH_CDR(obj))))
#define SCH_CDDAR(obj)   (SCH_CDR(SCH_CDR(SCH_CAR(obj))))
#define SCH_CDDDR(obj)   (SCH_CDR(SCH_CDR(SCH_CDR(obj))))

#define SCH_CAAAAR(obj)  (SCH_CAR(SCH_CAR(SCH_CAR(SCH_CAR(obj)))))
#define SCH_CAAADR(obj)  (SCH_CAR(SCH_CAR(SCH_CAR(SCH_CDR(obj)))))
#define SCH_CAADAR(obj)  (SCH_CAR(SCH_CAR(SCH_CDR(SCH_CAR(obj)))))
#define SCH_CAADDR(obj)  (SCH_CAR(SCH_CAR(SCH_CDR(SCH_CDR(obj)))))
#define SCH_CADAAR(obj)  (SCH_CAR(SCH_CDR(SCH_CAR(SCH_CAR(obj)))))
#define SCH_CADADR(obj)  (SCH_CAR(SCH_CDR(SCH_CAR(SCH_CDR(obj)))))
#define SCH_CADDAR(obj)  (SCH_CAR(SCH_CDR(SCH_CDR(SCH_CAR(obj)))))
#define SCH_CADDDR(obj)  (SCH_CAR(SCH_CDR(SCH_CDR(SCH_CDR(obj)))))
#define SCH_CDAAAR(obj)  (SCH_CDR(SCH_CAR(SCH_CAR(SCH_CAR(obj)))))
#define SCH_CDAADR(obj)  (SCH_CDR(SCH_CAR(SCH_CAR(SCH_CDR(obj)))))
#define SCH_CDADAR(obj)  (SCH_CDR(SCH_CAR(SCH_CDR(SCH_CAR(obj)))))
#define SCH_CDADDR(obj)  (SCH_CDR(SCH_CAR(SCH_CDR(SCH_CDR(obj)))))
#define SCH_CDDAAR(obj)  (SCH_CDR(SCH_CDR(SCH_CAR(SCH_CAR(obj)))))
#define SCH_CDDADR(obj)  (SCH_CDR(SCH_CDR(SCH_CAR(SCH_CDR(obj)))))
#define SCH_CDDDAR(obj)  (SCH_CDR(SCH_CDR(SCH_CDR(SCH_CAR(obj)))))
#define SCH_CDDDDR(obj)  (SCH_CDR(SCH_CDR(SCH_CDR(SCH_CDR(obj)))))

#define SCH_CADADDR(obj) (SCH_CAR(SCH_CDR(SCH_CAR(SCH_CDR(SCH_CDR(obj))))))

/* SYMBOL */
#define SCH_SYMBOL(str)      (sch_intern(str))
#define SCH_ID2NAME(id)      (sch_id2name(id))
char* sch_id2name(SchObj id);

/* ENCODING */
int get_char_size ( unsigned char c );

/* CHAR */
typedef struct SchCharRec {
    SchHeader    hd;
    unsigned int ch;
} SchChar;
SchObj make_char(unsigned int c);
#define SCH_CHAR_OBJ(obj)  ((SchChar*)obj)
#define SCH_CHAR(c)        (make_char(c))
#define CHAR_EQUALP(x1,x2) (((SCH_CHAR_OBJ(x1)->ch)==(SCH_CHAR_OBJ(x2)->ch))?SCH_TRUE:SCH_FALSE)

/* STRING */
typedef struct SchStringRec {
    SchHeader hd;
    char* buf;
    int   len;                  /* TODO size_t型に変更する */
    int   size;                 /* TODO size_t型に変更する */
} SchString;
#define SCH_STRING(s)        (make_string(s))
#define SCH_STRING_OBJ(obj)  ((SchString*)obj)


#define NUM_OF_SUBR 152


/* VECTOR */
typedef struct SchVectorRec {
    SchHeader hd;
    SchObj*   vec;
    int       size;
} SchVector;
SchObj make_vector(int size);
SchObj make_vector_fill(int size, SchObj fill);
#define SCH_VECTOR(size)         (make_vector(size))
#define SCH_VECTOR_F(size,fill)  (make_vector_fill(size,fill))
#define SCH_VECTOR_OBJ(obj)      ((SchVector*)obj)
#define SCH_VECTOR_REF(v,i)      ((SCH_VECTOR_OBJ(v)->vec[i]))
#define SCH_VECTOR_LEN(v)        ((SCH_VECTOR_OBJ(v)->size))

/* 使われてない？  read.cで使われてた */
typedef struct SchVecLListRec {
    SchObj obj;
    struct SchVecLListRec* next;
} SchVecLList;


/* PORT */
struct SchPortRec;
enum SchPortDirection
{
    SCH_PORT_INPUT,
    SCH_PORT_OUTPUT
};
enum SchPortType
{
    SCH_PORT_STRING,
    SCH_PORT_STREAM
};
enum SchPortStatus
{
    SCH_PORT_OPEN,
    SCH_PORT_CLOSED
};
typedef struct SchPortStreamRec
{
    FILE* fp;
    enum SchPortStatus  status;
} SchPortStream;
typedef struct SchPortStringRec
{
    char* str;
    char* current;
    int   index;
} SchPortString;
typedef struct SchPortRec
{
    SchHeader             hd;
    enum SchPortDirection direction;
    enum SchPortType      type;
    unsigned int          line;
    unsigned int (*sch_getc)(struct SchPortRec* port);
    char         (*sch_getbyte)(struct SchPortRec* port);
    void         (*sch_ungetc)(char c,struct SchPortRec* port);
    union
    {
        SchPortStream stream;
        SchPortString string;
    } src;
} SchPort;
#define SCH_PORT_OBJ(obj)    ((SchPort*)obj)
#define SCH_GETC(port)       ((*(port->sch_getc))(port))
#define SCH_UNGETC(c,port)   ((*(port->sch_ungetc))(c,port))
#define SCH_GETBYTE(port)    ((*(port->sch_getbyte))(port))

SchPort* make_input_port_string( char* str );

/* write */
SchPort* current_input_port();
SchPort* current_output_port();
void set_current_input_port(SchPort* port);
void set_current_output_port(SchPort* port);

void sch_write(FILE* f, SchObj obj);
#define SCH_WRITE(obj)    (sch_write(current_output_port()->src.stream.fp,obj))
#define SCH_DISPLAY(obj)  (sch_display(current_output_port()->src.stream.fp,obj))


/* exception */
#define EXCEPTION(msg) 			/* dummy  TODO IMPLEMENT THIS */


/* utilities */
#define SCH_MANGLE(id) sch_internal_##id

#define POP(_lst)                                                       \
  (SCH_MANGLE(tmp) = SCH_CAR(_lst), (_lst) = SCH_CDR(_lst), SCH_MANGLE(tmp))

#define SAFE_POP(_lst)                          \
  (PAIRP(_lst) ? (SCH_MANGLE(tmp) = SCH_CAR(_lst), (_lst) = SCH_CDR(_lst), SCH_MANGLE(tmp)) : _lst )

#define FOR_EACH_WHILE(_kar, _lst, _cond)       \
  while ((_cond) && ((_kar) = POP((_lst)),1))

#define FOR_EACH(_kar, _lst) FOR_EACH_WHILE((_kar),(_lst),PAIRP(_lst))

#define FOR_EACH_WHILE_2LISTS(_kar1,_lst1,_kar2,_lst2,_cond) \
  while((_cond) && ((_kar1)=POP((_lst1)),(_kar2)=POP((_lst2)),1))

#define FOR_EACH_2LISTS(_kar1,_lst1,_kar2,_lst2)     FOR_EACH_WHILE_2LISTS((_kar1),(_lst1),(_kar2),(_lst2),(PAIRP(_lst1)&&PAIRP(_lst2)))

#define SCH_QUEUE_INIT(_q)    (_q = SCH_NIL)
#define SCH_QUEUE_PUSH(_q,_x)                   \
  do {                                          \
    if ( NULLP(_q) ) {                          \
      _q = SCH_CONS(_x, SCH_NIL);               \
    } else {                                    \
      SchObj p;                                 \
      p = _q;                                           \
      while (1) {                                       \
        if( NULLP(SCH_CDR(p)) ) {                       \
          SCH_CDR(p) = SCH_CONS(_x, SCH_NIL);           \
          break;                                        \
        }                                               \
        POP(p);                                         \
      }                                                 \
    }                                                   \
  } while (0)

#define SCH_STACK_INIT(_q) (_q = SCH_NIL)
#define SCH_STACK_PUSH(_q,_x)                   \
  do {                                          \
    _q = SCH_CONS(_x, _q);                      \
  } while(0)


/* /\* for debug *\/ */
/* void print_tab(int n); */
/* void inspect_pair(SchPair* pair, int lv); */
/* void inspect_obj(SchObj obj, int lv); */


/* prototype */
SchObj make_pair(SchObj car, SchObj cdr);

SchObj make_string(char* s);
SchObj make_string_k_ch(int k, char c);
SchObj make_string_k(int k);
SchObj string_ref(SchString* str, int i);
void   string_set(SchString* str, int i, char c);
SchObj substring(SchObj str, int beg, int end);
SchObj string2list(SchObj str);
SchObj list2string(SchObj lst);
SchObj string_fill(SchObj str, SchObj chobj);

SchObj sch_intern(char* str);
SchObj list_ref(SchObj list, int index);
SchObj list_tail(SchObj list, int index);
SchObj list2vector(SchObj lst);
SchObj reverse(SchObj lst);

SchObj vector_fill(SchObj vec, SchObj obj);
SchObj vector2list(SchObj vec);
SchVecLList* make_vec_llist(void);
void   vec_push(SchVecLList* lst, SchObj x);
SchVector* make_vec_from_list(SchVecLList* lst, int size);
BOOL   is_input_port(SchPort* port);
BOOL   is_output_port(SchPort* port);
SchObj open_input_file(char* filename);
SchObj open_output_file(char* filename);
void   close_port(SchPort* port);
SchPort* make_input_port_stream(FILE* fp);
SchObj read_obj_via_port(SchPort* port);
void   sch_newline(SchPort* port);
int    ch_attr(int c);
SchObj sch_read (FILE* fp);
unsigned int read_char(SchPort* port);
void   sch_display(FILE* f, SchObj obj);
void   display_char(SchPort* port, SchObj obj);
SchObj sch_load(SchString* filename);

void   ensure_init_symtable(void);
void   init_reader(void);
void   init_port(void);
SchObj sch_read_string(char* str);
SchObj vm_compile(SchObj code);


#endif  /* not INCLUDE_MAME_H */

