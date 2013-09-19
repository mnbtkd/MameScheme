/*--------------------------------------------------------------------
  vm.c -  MameScheme ( Scheme Interpreter based on R5RS )
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
#include <signal.h>
#include <sys/time.h>
#include <errno.h>
#include "subr.h"

/* #define _VM_DEBUG */
int do_dump = 0;

extern int flag_use_profiler;

#define STACK_INIT_SIZE   60000
#define GLOBAL_INIT_SIZE   400



#define USE_DIRECT_THREADING 1

#define USE_ITIMER_PROF 0
#define SAMPLING_INTERVAL_MSEC 1
#define SAMPLING_INTERVAL_USEC (SAMPLING_INTERVAL_MSEC*1000)


#define DISPATCH_DIRECTLY(addr) \
  __asm__ __volatile__("jmp *%0" : : "r" (addr))

#if USE_DIRECT_THREADING
  typedef void* insn_type;
  #define VM_LOOP_BEG
  #define VM_DISPATCH_BLK   goto **x;
  #define LBL_ADDR(__LBL__) &&LB_##__LBL__
  #define DT_CASE(__LBL__)  LB_##__LBL__:
  #define DT_DEFAULT        DT_CASE(DEFAULT)
  #define DT_BREAK          DISPATCH_DIRECTLY(*x)
  #define VM_LOOP_END
#else
  typedef int insn_type;
  #define VM_LOOP_BEG       while(1){
  #define VM_DISPATCH_BLK   switch(*x)
  #define DT_CASE(__LBL__)  case __LBL__:
  #define DT_DEFAULT        default:
  #define DT_BREAK          continue
  #define VM_LOOP_END       }
#endif



enum INSTRUCTIONS {
    I_NONE = 99,    /*  0 */   /*  99 */
    I_HALT,         /*  1 */   /* 100 */
    I_LREF,         /*  2 */   /* 101 */
    I_FREF,         /*  3 */   /* 102 */
    I_GREF,         /*  4 */   /* 103 */
    I_UNBOX,        /*  5 */   /* 104 */
    I_CONST,        /*  6 */   /* 105 */
    I_CLOSE0,       /*  7 */   /* 106 */
    I_CLOSE1,       /*  8 */   /* 107 */
    I_CLOSE2,       /*  9 */   /* 108 */
    I_BOX,          /* 10 */   /* 109 */
    I_TEST,         /* 11 */   /* 110 */
    I_LSET,         /* 12 */   /* 111 */
    I_FSET,         /* 13 */   /* 112 */
    I_CONTI,        /* 14 */   /* 113 */
    I_NUATE,        /* 15 */   /* 114 */
    I_FRAME,        /* 16 */   /* 115 */
    I_PUSH,         /* 17 */   /* 116 */
    I_SHIFT,        /* 18 */   /* 117 */
    I_CALL,         /* 19 */   /* 118 */
    I_GSET,         /* 20 */   /* 119 */
    I_RETURN,       /* 21 */   /* 120 */
    I_JUMP,         /* 22 */   /* 121 */
    I_UNKNOWN       /* 23 */   /* 122 */
};

static SchObj SYM_NONE;
static SchObj SYM_HALT;
static SchObj SYM_LREF;
static SchObj SYM_FREF;
static SchObj SYM_GREF;
static SchObj SYM_UNBOX;
static SchObj SYM_CONST;
static SchObj SYM_CLOSE0;
static SchObj SYM_CLOSE1;
static SchObj SYM_CLOSE2;
static SchObj SYM_BOX;
static SchObj SYM_TEST;
static SchObj SYM_LSET;
static SchObj SYM_FSET;
static SchObj SYM_CONTI;
static SchObj SYM_NUATE;
static SchObj SYM_FRAME;
static SchObj SYM_PUSH;
static SchObj SYM_SHIFT;
static SchObj SYM_CALL;
static SchObj SYM_GSET;
static SchObj SYM_RETURN;
static SchObj SYM_JUMP;

static SchObj NONE;
static SchObj HALT;
static SchObj LREF;
static SchObj FREF;
static SchObj GREF;
static SchObj UNBOX;
static SchObj CONST;
static SchObj CLOSE0;
static SchObj CLOSE1;
static SchObj CLOSE2;
static SchObj BOX;
static SchObj TEST;
static SchObj LSET;
static SchObj FSET;
static SchObj CONTI;
static SchObj NUATE;
static SchObj FRAME;
static SchObj PUSH;
static SchObj SHIFT;
static SchObj CALL;
static SchObj GSET;
static SchObj RETURN;
static SchObj JUMP;
static SchObj UNKNOWN;


const char* INSTR_STR[]= {
    "NONE",       /*  0 */   /*  99 */
    "HALT",       /*  1 */   /* 100 */
    "LREF",       /*  2 */   /* 101 */
    "FREF",       /*  3 */   /* 102 */
    "GREF",       /*  4 */   /* 103 */
    "UNBOX",      /*  5 */   /* 104 */
    "CONST",      /*  6 */   /* 105 */
    "CLOSE0",     /*  7 */   /* 106 */
    "CLOSE1",     /*  8 */   /* 107 */
    "CLOSE2",     /*  9 */   /* 108 */
    "BOX",        /* 10 */   /* 109 */
    "TEST",       /* 11 */   /* 110 */
    "LSET",       /* 12 */   /* 111 */
    "FSET",       /* 13 */   /* 112 */
    "CONTI",      /* 14 */   /* 113 */
    "NUATE",      /* 15 */   /* 114 */
    "FRAME",      /* 16 */   /* 115 */
    "PUSH",       /* 17 */   /* 116 */
    "SHIFT",      /* 18 */   /* 117 */
    "CALL",       /* 19 */   /* 118 */
    "GSET",       /* 20 */   /* 119 */
    "RETURN",     /* 21 */   /* 120 */
    "JUMP",       /* 22 */   /* 121 */
    "UNKNOWN",    /* 23 */   /* 122 */
};

enum {
    ATT_CLOSURE,
    ATT_INTEGER,
    ATT_OBJECT,
    ATT_ADDRESS
};


/* --- stack -----------------------------------------------------------------------  */
SchObj stack[STACK_INIT_SIZE];

int push_stk ( SchObj x, int s )
{
    stack[s] = x;
    return ++s;
}

/* static SchObj index_st ( int s, int i) */
/* { */
/*     return stack[s-i-1]; */
/* } */

/* static void index_set( int s , int i, SchObj obj) */
/* { */
/*     stack[s-i-1] = obj; */
/* } */

SchObj* save_stack ( int s )
{
    SchObj* ret;
    int i;
    ret = SCH_MALLOC( sizeof(int)*(s+1) );
    ret[0] = s;
    for ( i=1 ; i<=s ; ++i ) {
        ret[i] = stack[i-1];
    }

    return ret;
}

int restore_stack( SchObj* v )
{
    int s, i;
    i = 0;
    s = v[0];
    while ( i < s ) {
        i = push_stk(v[i+1], i);
    }
    return s;
}

int shift_args(int n,int m, int s)
{
    int i;
    for ( i = n ; 0 < i ; --i ) {
        stack[s-i-m] = stack[s-i];
    }
    return s - m;
}

SchObj list_args(int s, int n)
{
    SchObj acc = SCH_NIL;
    for ( --n ; n >= 0 ; --n ) {
        acc = SCH_CONS(INDEX_ST(s,n),acc);
    }
    return acc;
}

SchObj list_args2(int s, int x, int n)
{
    SchObj acc = SCH_NIL;
    for ( --n,--x ; n >= 0 ; --n,--x ) {
        acc = SCH_CONS(INDEX_ST(s,x),acc);
    }
    return acc;
}

#ifdef _VM_DEBUG
static int stack_att[STACK_INIT_SIZE];
static int push_att( int type, int s )
{
    stack_att[s] = type;
    return ++s;
}
static int index_st_att( int s, int i)
{
    return stack_att[s-i-1];
}
 
static void index_set_att( int s , int i, int type)
{
    stack_att[s-i-1] = type;
}

int* save_stack_att( int s )
{
    int* ret;
    int i;
    ret = SCH_MALLOC( sizeof(int)*(s+1) );
    ret[0] = s;
    for ( i=1 ; i<=s ; ++i ) {
        ret[i] = stack_att[i-1];
    }

    return ret;
}

int restore_stack_att( int* v )
{
    int s, i;
    i = 0;
    s = v[0];
    while ( i < s ) {
        i = push_att(v[i+1], i);
    }
    return s;
}

int shift_args_att( int n, int m, int s )
{
    int i;
    for ( i = n ; 0 < i ; --i ) {
        stack_att[s-i-m] = stack_att[s-i];
    }
    return s - m;
}

int shift_up_args_att(int n,int m,int s)
{
    int i;
    for ( i = 1 ; i <= n ; ++i ) {
        stack_att[s - i + m] = stack_att[s - i];
    }
    return s + m;
}

SchObj list_args_att( int s, int n )
{
    SchObj acc = SCH_NIL;
    for ( --n ; n >= 0 ; --n ) {
        acc = SCH_CONS(index_st_att(s,n),acc);
    }
    return acc;
}

SchObj list_args2_att( int s, int x, int n )
{
    SchObj acc = SCH_NIL;
    for ( --n,--x ; n >= 0 ; --n,--x ) {
        acc = SCH_CONS(index_st_att(s,x),acc);
    }
    return acc;
}

#endif

/* --- box ------------------------------------------------------- */
typedef struct BoxRec {
    SchHeader hd;
    SchObj obj;
} Box;

static Box* box(SchObj obj)
{
    Box* b = SCH_MALLOC(sizeof(Box));
    b->hd.flags = T_BOX;
    b->obj = obj;
    return b;
}

static void set_box(Box* box, SchObj obj)
{
    box->obj = obj;
}

static SchObj unbox(Box* box)
{
    return (box->obj);
}
/* --- display closure ------------------------------------------- */
DisplayClosure* closure ( int type, SchObj* body_top, int free_var_size, int argnum, int body_size, int s )
{
    DisplayClosure* ret;
    int i;
    i = 0;
    
    ret = SCH_MALLOC(sizeof(DisplayClosure));
    ret->name = "N/A";
    ret->hd.flags = T_LAMBDA;
    ret->argtype = type;
    ret->argnum  = argnum;
    ret->u.body.body_size = body_size;
    ret->u.body.body_top  = body_top;
    ret->vars_size = free_var_size;

    ret->vars = SCH_MALLOC(sizeof(SchObj)*(ret->vars_size));
    while ( i < free_var_size ) {
        ret->vars[i] = INDEX_ST(s, i);
        ++i;
    }
    return ret;
}

/* --- continuation ------------------------------------------------------------------ */
SchObj continuation ( SchObj* x, int s )
{
    SchObj* ps;
    SchObj* stack = save_stack(s);
/* (list 'lref 0 (list 'nuate (save-stack s) '(return 0))) */

    ps = SCH_MALLOC(sizeof(SchObj)*6);
    ps[0] = LREF;
    ps[1] = 0;
    ps[2] = NUATE;
    ps[3] = (SchObj)stack;
    ps[4] = RETURN;
    ps[5] = 0;

    return (SchObj)closure(CLOSE0,ps,0,-1,6,s);
}

/* --- global definition ------------------------------------------------------------------ */
static SchObj globals[GLOBAL_INIT_SIZE];
static SchObj index_global(int index)
{
    if ( index < GLOBAL_INIT_SIZE ) {
        return globals[index];
    } else {
        EXCEPTION("invalid index");
    }
}
static SchObj assign_global(int index, SchObj x)
{
    if ( index < GLOBAL_INIT_SIZE ) {
        globals[index] = x;
    } else {
        EXCEPTION("no more space for global vriables");
    }
    return SCH_UNDEFINE;
}

/* DisplayClosure* closure ( int type, SchObj* body_top, int free_var_size, int argnum, int body_size, int s ) */
/* { */
/*     DisplayClosure* ret; */
/*     int i; */
/*     i = 0; */
    
/*     ret = SCH_MALLOC(sizeof(DisplayClosure)); */

/*     ret->hd.flags = T_LAMBDA; */
/*     ret->argtype = type; */
/*     ret->argnum  = argnum; */
/*     ret->u.body.body_size = body_size; */
/*     ret->u.body.body_top  = body_top; */
/*     ret->vars_size = free_var_size; */

/*     ret->vars = SCH_MALLOC(sizeof(SchObj)*(ret->vars_size)); */
/*     while ( i < free_var_size ) { */
/*         ret->vars[i] = INDEX_ST(s, i); */
/*         ++i; */
/*     } */
/*     return ret; */
/* } */
static DisplayClosure* closure2( int type,
                                 int body_size,
                                 SchObj* body_top,
                                 int var_size,
                                 SubrPnt* subrs,
                                 char** names )
{
    DisplayClosure* ret;
    int i;
    ret = SCH_MALLOC(sizeof(DisplayClosure));
    ret->hd.flags = T_LAMBDA;
    ret->argtype  = type;
    ret->u.body.body_size = body_size;

    ret->u.body.body_top = SCH_MALLOC(sizeof(SchObj)*body_size);
    for ( i = 0 ; i < body_size ; ++i ){
        ret->u.body.body_top[i] = body_top[i];
    }

    ret->vars_size = var_size;
    ret->vars = SCH_MALLOC(sizeof(SchObj)*var_size);
    for (i = 0 ; i < var_size ; ++i ) {
/*         ret->vars[i] = make_subr(subrs[i],"dummy_name"); */
        ret->vars[i] = (SchObj)make_subr(subrs[i],names[i]);
    }

    return ret;
}

/* TODO code26[] などの変数をstaticにすべき?? */
static void init_globals(void)
{
    static int initialized = 0;
    int idx = 0;

    if ( initialized ) {
        return;
    }

    while ( idx < GLOBAL_INIT_SIZE ) {
        globals[idx] = SCH_UNBOUND;
        idx++;
    }

#include "./compile.c"

    initialized = 1;
}


/* --- debug utilities ----------------------------------------------------------------*/

static char* extend_strbuf(char* buf, int size)
{
    char * tmp;
    tmp = SCH_MALLOC(size);
    strcpy(tmp,buf);
    SCH_FREE(buf);
    return tmp;
}
static char* to_s(SchObj obj)
{
    char * str;/*  = SCH_MALLOC(128); */

    if ( obj == 0 ) {
        /* stackをダンプする時に4とか入っているとこの関数で落ちるので */
        str = SCH_MALLOC(128);
        sprintf( str, "%ld", obj );
        return str;
    }

    if ( SCH_TYPE(obj) != T_PAIR ) {
        str = SCH_MALLOC(128);
    }

    switch(SCH_TYPE(obj))
    {
    case T_MISC_NIL:      sprintf(str,"#<nil>"  ); break;
    case T_MISC_FALSE:    strcpy(str,"#<false>"); break;
    case T_MISC_TRUE:     strcpy(str,"#<true>" ); break;
    case T_MISC_EOF:      strcpy(str,"#<eof>"  ); break;
    case T_MISC_UNBOUND:  strcpy(str,"#<unbnd>"); break;
    case T_MISC_UNDEFINE: strcpy(str,"#<undef>"); break;
    case T_MISC_DOT:      strcpy(str,"#<dot>"  ); break;
    case T_MISC_KOKKA:    strcpy(str,"#<kokka" ); break;
    case T_FIXNUM:        sprintf(str,"#<fixnum %ld>",FIX2INT(obj)); break;
    case T_BIGNUM:        sprintf(str,"#<bignum %s>",bignum2str(obj,10)); break;
    case T_RATIONAL:      {
        SchObj n = SCH_RATIONAL_OBJ(obj)->numerator;
        SchObj d = SCH_RATIONAL_OBJ(obj)->denominator;
        if ( FIXNUMP(n) && FIXNUMP(d) ) {
            sprintf(str,"#<rational %d/%d>",FIX2INT(n),FIX2INT(d));
        } else if ( FIXNUMP(n) ) {
            sprintf(str,"#<rational %d/%s>",FIX2INT(n),bignum2str(d,10));
        } else if ( FIXNUMP(d) ) {
            sprintf(str,"#<rational %s/%d>",bignum2str(n,10),FIX2INT(d));
        } else {
            sprintf(str,"#<rational %s/%s>",bignum2str(n,10),bignum2str(d,10));
        }
        break;
    }
    case T_SYMBOL:        sprintf(str,"#<symbol %s>",SCH_ID2NAME(obj)); break;
    case T_STRING:        sprintf(str,"#<string %s>",SCH_STRING_OBJ(obj)->buf); break;
    case T_LAMBDA:        sprintf(str,"#<lambda >"); break;
    case T_BOX:           sprintf(str,"#<box>"); break;
    case T_SUBR:          sprintf(str,"#<subr %s>",((DisplayClosure*)obj)->name); break;
    case T_PAIR:{
        char * car = to_s(SCH_CAR(obj));
        char * cdr = to_s(SCH_CDR(obj));
        char * fmt = "#<pair [%s][%s]>";
/*         tmp = realloc(str, strlen(car) + strlen(cdr) + strlen(fmt) - 4 + 1); */
/*         if ( tmp ) { */
/*             MSF_FREE(str); */
/*             str = tmp; */
/*         } else { */
/*             printf("failed to reallocate.\n"); */
/*         } */

/*         SCH_FREE(str); */

        str = SCH_MALLOC( strlen(car) + strlen(cdr) + strlen(fmt) - 4 + 1 );
        sprintf(str,fmt,car,cdr);
        SCH_FREE(car);
        SCH_FREE(cdr);
    } break;
    case T_VECTOR: sprintf(str,"#<vector>"); break;
    default:              strcpy(str,"#<unknown obj>"); break;
    }
    str[127] = '\0';
    return str;
}

char* ins_s ( SchObj* i, int size )
{
    char* ret = SCH_MALLOC(192*size);
    char* str = SCH_MALLOC(192);
    char* tmp;
    int   j;

    ret[0] = '\0';

    for ( j = 0 ; j < size ; ++j ) {
        switch( *i ){
            /* operand 0 */
        case I_NONE:
        case I_PUSH:
        case I_UNBOX:
        case I_CONTI: {
            sprintf(str,"%s ", INSTR_STR[(*i++)-99]);
        } break;

        case I_HALT: {
            sprintf(str,"%s ", INSTR_STR[(*i++)-99]);
            ret = strcat(ret,str);
            return ret;
        } break;

            /* operand 1 */
        case I_LREF:
        case I_FREF:
        case I_GREF:
        case I_TEST:
        case I_FRAME:
        case I_CALL:
        case I_RETURN:
        case I_BOX:
        case I_LSET:
        case I_FSET:
        case I_GSET:
        case I_JUMP:
        case I_NUATE: {
            sprintf(str,"%s %ld ",INSTR_STR[(*i)-99],*(i+1));
            i += 2;
        } break;

        case I_CONST: {
            tmp = to_s(*(i+1));
            sprintf(str,"%s %s ",INSTR_STR[(*i)-99],tmp);
            SCH_FREE(tmp);
            i += 2;
        } break;


            /* operand 2 */
        case I_SHIFT: {
            sprintf(str,"%s %ld %ld ",INSTR_STR[(*i)-99],*(i+1), *(i+2));
            i += 3;
        } break;


            /* operand 3 */
        case I_CLOSE0:
        case I_CLOSE1:
        case I_CLOSE2: {
            sprintf(str,"%s %ld %ld %ld ",INSTR_STR[(*i)-99],*(i+1), *(i+2), *(i+3));
            i += 4;
        } break;

        default: break;
        }
        ret = strcat(ret,str);
    }

    return ret;
}

static void objdump(SchObj obj)
{
    char* str = to_s(obj);
    printf("%s\n",str);
    SCH_FREE(str);
}

char* dump_stack_s(int sp, int size)
{
    int i;
    int bufsize = 2048;
    char* buf = SCH_MALLOC(bufsize);
    char tmp[512];
    char* obj_s;

    sprintf(buf,"\n--- dump_stack  sp[%d] size[%d] ---\n", sp, size);
    for ( i = 0 ; i < size && sp-i > 0 ; ++i ) {
        SchObj obj  = INDEX_ST(sp-i,0);
#ifdef _VM_DEBUG
        int    type = index_st_att(sp-i,0);
#else
        int type = 0;           /* dummy */
#endif
        if ( type == ATT_INTEGER ){
            obj_s = SCH_MALLOC(32);
            sprintf(obj_s,"%ld",obj);
        } else if ( type == ATT_ADDRESS ) {
            obj_s = SCH_MALLOC(512);
            sprintf(obj_s,"%ld -> %s",obj,ins_s((SchObj*)obj,4));
        } else {
            obj_s = to_s(obj);
        }
        sprintf(tmp,"  %04d %08lx | %s\n",sp-1-i,obj,obj_s);
        if ( bufsize <= (strlen(tmp) + strlen(buf)) ) {
            bufsize += 2048;
            buf = extend_strbuf(buf,bufsize);
        }
        buf = strcat(buf,tmp);
        SCH_FREE(obj_s);
    }
    return buf;
}

void dump_stack(int sp, int size)
{
    char* buf = dump_stack_s(sp,size);
    printf(buf);
    SCH_FREE(buf);
}

void dpr2( SchObj a, SchObj* x, int pc, int f, DisplayClosure* c, int s, int s_att )
{
    char* a_str = to_s(a);
    char* x_str = ins_s(x,6);
    char* i_str = ins_s(x,1);

    printf("---------------------------------------------------\n[%s]\n",i_str);
    if (a){
        printf("  a --- %lx  %s   type:%d\n", a, a_str, SCH_TYPE(a) );
    } else {
        printf("  a --- 0   0\n");
    }
    printf("  x --- %p      %s\n", x, x_str);
    printf("  pc--- %d\n", pc);
    printf("  f --- %d\n", f );
    printf("  c --- %p\n", c );
    printf("  s --- %d\n", s );
    printf("  sa--- %d\n", s_att );
    SCH_FREE(a_str);
    SCH_FREE(i_str);
    SCH_FREE(x_str);
    dump_stack(s,s);
}


/* --- profiler ----------------------------------------------------------------*/

#include "st.h"
static int       total_samples = 0;
static st_table* smpl_tbl      = 0;
static st_table* call_tbl      = 0;

typedef struct CallChainRec {
    char* fname;
    struct CallChainRec* next;
} CallChain;

static CallChain* call_chain_top = 0;

void push_fname(char* fname)
{
    CallChain* top = SCH_MALLOC(sizeof(CallChain));
    top->fname = fname;

    if ( call_chain_top ) {
        top->next = call_chain_top;
    } else {
        top->next = NULL;
    }

    call_chain_top = top;
}

char* pop_fname()
{
    char * fname = NULL;
    CallChain* old;
    if ( call_chain_top ) {
        fname = call_chain_top->fname;
        old   = call_chain_top;
        call_chain_top = call_chain_top->next;
        SCH_FREE(old);
    }
    return fname;
}

char* top_fname()
{
    if ( call_chain_top ) {
        return call_chain_top->fname;
    } else {
        return NULL;
    }
}

void signal_handler(int signo)
{
    char* fname = top_fname();
    if ( fname ) {
        int i=0;
        if ( ! st_lookup(smpl_tbl, fname, &i) ) {
            st_insert(smpl_tbl,fname,1);
        } else {
            st_insert(smpl_tbl,fname,++i);
        }
    }
    total_samples++;
}

void start_profiler()
{
    /* profiler */
    struct sigaction act;
    act.sa_handler = &signal_handler; /* set signal_handler */
    act.sa_flags   = SA_RESTART;      /* restart system call after signal handler  */
#if USE_ITIMER_PROF
    if ( sigaction( SIGPROF, &act, NULL ) != 0 ) {
#else
    if ( sigaction( SIGALRM, &act, NULL ) != 0 ) {
#endif
        fprintf( stderr, "sigaction failed\n" );
        exit( 1 );
    }

    call_tbl = st_init_strtable();
    smpl_tbl = st_init_strtable();
    total_samples = 0;

    struct itimerval timer, old;

    timer.it_interval.tv_sec  = 0;
    timer.it_interval.tv_usec = SAMPLING_INTERVAL_USEC;
    timer.it_value.tv_sec  = 0;
    timer.it_value.tv_usec = SAMPLING_INTERVAL_USEC;

#if USE_ITIMER_PROF
    if ( setitimer(ITIMER_PROF, &timer, &old) != 0 ) {
#else
    if ( setitimer(ITIMER_REAL, &timer, &old) != 0 ) {
#endif
        printf("settimer failed.  ");
        if ( errno == EFAULT ) {
            printf("EFAULT\n");
        }
        if ( errno == EINVAL ) {
            printf("EINVAL\n");
        }
    }

}

void stop_profiler()
{
    struct itimerval timer;

    timer.it_interval.tv_sec  = 0;
    timer.it_interval.tv_usec = 0;
    timer.it_value.tv_sec  = 0;
    timer.it_value.tv_usec = 0;

#if USE_ITIMER_PROF
    setitimer(ITIMER_PROF,&timer,NULL);
#else
    setitimer(ITIMER_REAL,&timer,NULL);
#endif

}

enum st_retval show_prof_a_line(char* key, int* rec)
{
    int i;
    double per;
    if ( key && st_lookup(smpl_tbl,key,&i) ) {
        per = ((double)i) / total_samples * 100;
    } else {
        i   = 0;
        per = 0;
    }
    printf("%-26s %10d %10d ( %5.2f %% )\n",key,rec,i,per);
    return ST_CONTINUE;
}

void show_prof( char* title )
{
    printf("\n\n------------------------ [ %-16s ] -----------------------\n", title);
    printf("total %d samples, %f seconds\n\n", total_samples, ((double)(total_samples*SAMPLING_INTERVAL_USEC ))/1000000);
    printf("name                        num calls          total samples\n");
    printf("--------------------------+----------+----------------------+\n");
    st_foreach(call_tbl,show_prof_a_line);
}

enum st_retval count_size(char* key, int* rec, char* acc)
{
    *((int*)acc) += 1;
    return ST_CONTINUE;
}
enum st_retval make_keys(char* key, char* rec, char* base)
{
    static int index=0;
    if (base) {
        ((char**)base)[index] = key;
        index++;
    }else{
        index = 0;
    }
    return ST_CONTINUE;
}
int compare_sample_num(void* arg1, void* arg2)
{
    int i1, i2;
    if ( !st_lookup(smpl_tbl,*(char**)arg1,&i1) ) {
        i1 = 0;
    }
    if ( !st_lookup(smpl_tbl,*(char**)arg2,&i2) ) {
        i2 = 0;
    }
    return (i2 - i1);
}
void show_prof_sorted( char* title )
{
    int size = 0;
    int i;
    char** base;

    st_foreach(call_tbl,count_size,&size);
    base = SCH_MALLOC(sizeof(char*) * size);
    st_foreach(call_tbl,make_keys,base);
    make_keys(NULL,NULL,NULL);

    qsort(base,size,sizeof(char*),(int (*)(const void*,const void*))compare_sample_num);

    printf("\n\n------------------------ [ %-16s ] -----------------------\n", title);
    printf("total %d samples, %f seconds\n\n", total_samples, ((double)(total_samples*SAMPLING_INTERVAL_USEC ))/1000000);
    printf("name                        num calls          total samples\n");
    printf("--------------------------+----------+----------------------+\n");

    for ( i = 0 ; i < size ; ++i ) {
        int num,rec;
        double per;
        char* key = base[i];
        if ( key && st_lookup(smpl_tbl,key,&num) ) {
            per = ((double)num) / total_samples * 100;
        } else {
            num = 0;
            per = 0;
        }
        if ( !st_lookup(call_tbl,key,&rec)) {
            rec = 0;
        }
        printf("%-26s %10d %10d ( %5.2f %% )\n",key,rec,num,per);
    }


}


/* --- vm ---------------------------------------------------------------------------- */
SchObj vm ( int demand_insn_tbl, SchObj* x0, DisplayClosure* c0, int size, int sp, int fp )
{
    int              pc = 0;            /* program counter            EIP? */
    volatile int     s  = sp;           /* stack top                  ESP? */
    volatile SchObj  a  = SCH_UNDEFINE; /* accumulator                EAP? */
    volatile int     f  = fp;           /* frame ( for dynamic link ) ESB? */
    int              instr;
    SchObj *         last_addr = 0;

    volatile SchObj *         volatile x = x0;
    volatile DisplayClosure * volatile c = c0;

#ifdef _VM_DEBUG
    int    s_att = sp;
#endif

#if USE_DIRECT_THREADING
    if ( demand_insn_tbl ) {
        NONE    = (SchObj)LBL_ADDR(I_NONE);
        HALT    = (SchObj)LBL_ADDR(I_HALT);
        LREF    = (SchObj)LBL_ADDR(I_LREF);
        FREF    = (SchObj)LBL_ADDR(I_FREF);
        GREF    = (SchObj)LBL_ADDR(I_GREF);
        UNBOX   = (SchObj)LBL_ADDR(I_UNBOX);
        CONST   = (SchObj)LBL_ADDR(I_CONST);
        CLOSE0  = (SchObj)LBL_ADDR(I_CLOSE0);
        CLOSE1  = (SchObj)LBL_ADDR(I_CLOSE1);
        CLOSE2  = (SchObj)LBL_ADDR(I_CLOSE2);
        BOX     = (SchObj)LBL_ADDR(I_BOX);
        TEST    = (SchObj)LBL_ADDR(I_TEST);
        LSET    = (SchObj)LBL_ADDR(I_LSET);
        FSET    = (SchObj)LBL_ADDR(I_FSET);
        CONTI   = (SchObj)LBL_ADDR(I_CONTI);
        NUATE   = (SchObj)LBL_ADDR(I_NUATE);
        FRAME   = (SchObj)LBL_ADDR(I_FRAME);
        PUSH    = (SchObj)LBL_ADDR(I_PUSH);
        SHIFT   = (SchObj)LBL_ADDR(I_SHIFT);
        CALL    = (SchObj)LBL_ADDR(I_CALL);
        GSET    = (SchObj)LBL_ADDR(I_GSET);
        RETURN  = (SchObj)LBL_ADDR(I_RETURN);
        JUMP    = (SchObj)LBL_ADDR(I_JUMP);
        UNKNOWN = (SchObj)LBL_ADDR(I_UNKNOWN);
        return SCH_NIL;
    }
#endif


    if ( 0 < size ) {
        last_addr = &x[size-1];
    }

#ifdef _VM_DEBUG
    printf("\n========================================\n");
    printf("[ VM CALLED WITH LIMIT %p ]\n", last_addr);
    printf("========================================\n");
#endif

    instr = (int)*x;

    VM_LOOP_BEG

#ifdef _VM_DEBUG
        if (do_dump) {
            dpr2(a,x,pc,f,c,s,s_att);
        }
#endif
        ++pc;

        VM_DISPATCH_BLK {

        DT_CASE(I_HALT) {
            return a;
        }
        DT_CASE(I_NONE) {
            ++x;
        }DT_BREAK;
        DT_CASE(I_CONST) {
            a = *(++x);
            instr = (int)*(++x);
        } DT_BREAK;
        DT_CASE(I_PUSH) {
            s = push_stk(a,s);
#ifdef _VM_DEBUG
            s_att = push_att(ATT_OBJECT,s_att);
#endif
            instr = (int)*(++x);
        } DT_BREAK;
        DT_CASE(I_FRAME) {
            int size = *(x+1);
            s = push_stk( (SchObj)(x+size+2),
                          push_stk( (SchObj)f,
                                    push_stk( (SchObj)c,
                                              s )));
#ifdef _VM_DEBUG
            s_att = push_att(ATT_ADDRESS,push_att(ATT_INTEGER, push_att(ATT_CLOSURE,s_att)));
#endif
            x+=2;
            instr = (int)*x;
        } DT_BREAK;
        DT_CASE(I_FREF) {
            a = c->vars[*++x];
            instr = (int)*++x;
        } DT_BREAK;
        DT_CASE(I_LREF) {
            a = INDEX_ST(f,*++x);
            instr = (int)*++x;
        } DT_BREAK;
        DT_CASE(I_GREF) {
            a = index_global(*++x);

            if ( UNBOUNDP(a) ) {
                char* name = SCH_ID2NAME( list_ref( globals[INDEX_GLOBALS_N],
                                                    (list_length(globals[INDEX_GLOBALS_N]) - 1 - (*x)) ) );
                printf("*** ERROR: unbound variable: %s\n",name);
                return SCH_UNDEFINE;
            }

            if ( flag_use_profiler ) {
                if ( LAMBDAP(a) ) {
                    ((DisplayClosure*)a)->name = SCH_ID2NAME( list_ref( globals[INDEX_GLOBALS_N],
                                                                        (list_length(globals[INDEX_GLOBALS_N]) - 1 - (*x)) ) );
                }
            }

            instr = (int)*++x;
        } DT_BREAK;
        DT_CASE(I_TEST) {
            x++;
            int then_size = *x++;
            if (a == SCH_FALSE) {
                x += then_size;
            }
            instr = (int)*x;
        } DT_BREAK;
        DT_CASE(I_BOX) {
            SchObj obj = INDEX_ST(s,*++x);
            Box*   b   = box(obj);
            INDEX_SET(s,*x,(SchObj)b);
#ifdef _VM_DEBUG
            index_set_att(s_att,*x,ATT_OBJECT);
#endif
            instr = (int)*++x;
        } DT_BREAK;
        DT_CASE(I_UNBOX) {
            a = unbox((Box*)a);
            instr = (int)*++x;
        } DT_BREAK;
        DT_CASE(I_LSET) {
            Box* b = (Box*)INDEX_ST(f,*++x);
            set_box(b,a);
            instr = (int)*++x;
        } DT_BREAK;
        DT_CASE(I_GSET) {
            a = assign_global(*++x,a);
            instr = (int)*++x;
        } DT_BREAK;
        DT_CASE(I_FSET) {
            set_box((Box*)c->vars[*++x], a);
            instr = (int)*++x;
        } DT_BREAK;
#if USE_DIRECT_THREADING
        DT_CASE(I_CLOSE0) instr = (SchObj)LBL_ADDR(I_CLOSE0); goto CLOSE_DCL;
        DT_CASE(I_CLOSE1) instr = (SchObj)LBL_ADDR(I_CLOSE1); goto CLOSE_DCL;
        DT_CASE(I_CLOSE2) instr = (SchObj)LBL_ADDR(I_CLOSE2); goto CLOSE_DCL;
        CLOSE_DCL:
        {
#else
        DT_CASE(I_CLOSE0)
        DT_CASE(I_CLOSE1)
        DT_CASE(I_CLOSE2) {
#endif
            int free_var_size = *++x;
            int argnum        = *++x;
            int body_size = *++x;

            a = (SchObj)closure(instr, ++x, free_var_size, argnum, body_size, s);
            x += body_size;
            s -= free_var_size;
#ifdef _VM_DEBUG
            s_att -= free_var_size;
#endif
            instr = (int)*x;
        } DT_BREAK;
        DT_CASE(I_CALL) {
            DisplayClosure* closure = (DisplayClosure*)a;

/*             if ( flag_use_profiler ) { */
/*                 char* cname = closure->name; */
/*                 if ( cname ) { */
/*                     int i=0; */
/*                     if ( ! st_lookup(call_tbl, cname, &i) ) { */
/*                         st_insert(call_tbl,cname,1); */
/*                     } else { */
/*                         st_insert(call_tbl,cname,++i); */
/*                     } */
/*                 } */
/*             } */

            if ( LAMBDAP(closure) ) {

                if ( flag_use_profiler ) {
                    char* cname = closure->name;
                    if ( cname ) {
                        int i=0;
                        if ( ! st_lookup(call_tbl, cname, &i) ) {
                            st_insert(call_tbl,cname,1);
                        } else {
                            st_insert(call_tbl,cname,++i);
                        }
                    }
                    push_fname(closure->name);
                }

                if ( closure->argtype == CLOSE0 ) {
                    x = closure->u.body.body_top;
                    f = s;
                    c = closure;
                } else if ( closure->argtype == CLOSE1 ) {
                    int    diff = *(x+1) - 1;
                    INDEX_SET(s,diff,list_args(s,*(x+1)));
                    s = s - diff;
#ifdef _VM_DEBUG
                    s_att = s;
#endif
                    x = closure->u.body.body_top;
                    f = s;
                    c = closure;
                } else if ( closure->argtype == CLOSE2 ) {
                    int diff = *(x+1) - closure->argnum;
                    INDEX_SET( s,
                               (*(x+1)) - 1,
                               list_args2( s,
                                           *(x+1),
                                           diff+1 ) );
                    s = shift_args( closure->argnum - 1, diff, s);
#ifdef _VM_DEBUG
                    s_att = s;
#endif
                    x = closure->u.body.body_top;
                    f = s;
                    c = closure;
                } else {
                    printf("ERROR Invalid closure...\n");
                    exit(0);
                }
                instr = (int)*x;
                DT_BREAK;

            } else {
                if ( flag_use_profiler ) { /* subrがcallされたら(subrはプロファイルのサンプリングに入れないので、以前のクロージャ名でpushしておく) */
                    char* name  = 0;
                    name = top_fname();
                    push_fname(name);
                }
                a = (*(closure->u.subr))(s, *++x);

                if ( x == last_addr ) {
#ifdef _VM_DEBUG
                    printf("\n--------------------------------\n");
                    printf("reached to the last instruction. \n");
                    printf("--------------------------------\n");
#endif
                    return a;
                }
                instr = *++x;
                DT_BREAK;
            }
        }
        DT_CASE(I_SHIFT) {
            int n = *++x;
            int m = *++x;
            s = shift_args(n,m,s);
#ifdef _VM_DEBUG
            s_att = s;
#endif
            instr = (int)*++x;
        } DT_BREAK;
        DT_CASE(I_RETURN) {
            s -= *++x;

            if ( flag_use_profiler ) {
                pop_fname();
            }

            if ( x == last_addr ) { /* closureの中身を実行中の場合、最後のインストラクションを実行後に即リターンする */
#ifdef _VM_DEBUG
                printf("\n--------------------------------\n");
                printf("reached to the last instruction. \n");
                printf("--------------------------------\n");
#endif
                return a;
            }

            x  = ((SchObj*)        INDEX_ST(s,0));
            f  = ((int)            INDEX_ST(s,1));
            c  = ((DisplayClosure*)INDEX_ST(s,2));
            s -= 3;
#ifdef _VM_DEBUG
            s_att = s;
#endif
            instr = (int)*x;
        } DT_BREAK;
        DT_CASE(I_CONTI) {
            a = continuation(x, s); /* TODO to fix continuation */
            instr = (int)*++x;
        } DT_BREAK;
        DT_CASE(I_NUATE) {
            s = restore_stack(*++x);
#ifdef _VM_DEBUG
            s_att = restore_stack_att(*x);
#endif
            instr = (int)*++x;
        } DT_BREAK;
        DT_CASE(I_JUMP) {
            size = *(x+1);
            x+= 2+size;
            instr = (int)*x;
        } DT_BREAK;
        DT_CASE(I_UNKNOWN)
        DT_DEFAULT {
            printf("Internal Error: unknown instruction <%d>.\n",instr);
            printf("\n\n");
            return 0;
        }
        }
    VM_LOOP_END
    return a;
}


/* int vm_main ( int argc, char** argv ) */
/* { */
/*     SchObj ret; */
/*     SchObj pickup[] = { */
/*     }; */


/*     SubrPnt pnt[NUM_OF_SUBR]; */
/*     char*   name[NUM_OF_SUBR]; */
/*     DisplayClosure* c = make_subrs(pnt,name); */

/* /\*     vm(0,kode,c,-1,0,0); *\/ */
/* /\*     exit(0); *\/ */

/* #ifdef _VM_DEBUG */
/*     printf("initial closure --- %d\n",c); */
/* #endif */

/*     ret = vm(0,pickup, c, -1, 0, 0); */
/*     SCH_WRITE(ret); */
/*     printf("\n"); */

/*     exit(0); */
/* } */

void init_instructions()
{
/*     void** tbl; */
    SYM_NONE   = SCH_SYMBOL("none");
    SYM_HALT   = SCH_SYMBOL("halt");
    SYM_LREF   = SCH_SYMBOL("lref");
    SYM_FREF   = SCH_SYMBOL("fref");
    SYM_GREF   = SCH_SYMBOL("gref");
    SYM_UNBOX  = SCH_SYMBOL("unbox");
    SYM_CONST  = SCH_SYMBOL("const");
    SYM_CLOSE0 = SCH_SYMBOL("close0");
    SYM_CLOSE1 = SCH_SYMBOL("close1");
    SYM_CLOSE2 = SCH_SYMBOL("close2");
    SYM_BOX    = SCH_SYMBOL("box");
    SYM_TEST   = SCH_SYMBOL("test");
    SYM_LSET   = SCH_SYMBOL("lset");
    SYM_FSET   = SCH_SYMBOL("fset");
    SYM_CONTI  = SCH_SYMBOL("conti");
    SYM_NUATE  = SCH_SYMBOL("nuate");
    SYM_FRAME  = SCH_SYMBOL("frame");
    SYM_PUSH   = SCH_SYMBOL("push");
    SYM_SHIFT  = SCH_SYMBOL("shift");
    SYM_CALL   = SCH_SYMBOL("call");
    SYM_GSET   = SCH_SYMBOL("gset");
    SYM_RETURN = SCH_SYMBOL("return");
    SYM_JUMP   = SCH_SYMBOL("jump");

#if USE_DIRECT_THREADING
    vm(1,NULL,NULL,0,0,0);
#else
    NONE    = I_NONE;
    HALT    = I_HALT;
    LREF    = I_LREF;
    FREF    = I_FREF;
    GREF    = I_GREF;
    UNBOX   = I_UNBOX;
    CONST   = I_CONST;
    CLOSE0  = I_CLOSE0;
    CLOSE1  = I_CLOSE1;
    CLOSE2  = I_CLOSE2;
    BOX     = I_BOX;
    TEST    = I_TEST;
    LSET    = I_LSET;
    FSET    = I_FSET;
    CONTI   = I_CONTI;
    NUATE   = I_NUATE;
    FRAME   = I_FRAME;
    PUSH    = I_PUSH;
    SHIFT   = I_SHIFT;
    CALL    = I_CALL;
    GSET    = I_GSET;
    RETURN  = I_RETURN;
    JUMP    = I_JUMP;
    UNKNOWN = I_UNKNOWN;
#endif

}

int ins_val(SchObj sym)
{
#if USE_DIRECT_THREADING
    if      ( SYM_NONE   == sym ) { return NONE; }
    else if ( SYM_HALT   == sym ) { return HALT; }
    else if ( SYM_LREF   == sym ) { return LREF; }
    else if ( SYM_FREF   == sym ) { return FREF; }
    else if ( SYM_GREF   == sym ) { return GREF; }
    else if ( SYM_UNBOX  == sym ) { return UNBOX; }
    else if ( SYM_CONST  == sym ) { return CONST; }
    else if ( SYM_CLOSE0 == sym ) { return CLOSE0; }
    else if ( SYM_CLOSE1 == sym ) { return CLOSE1; }
    else if ( SYM_CLOSE2 == sym ) { return CLOSE2; }
    else if ( SYM_BOX    == sym ) { return BOX; }
    else if ( SYM_TEST   == sym ) { return TEST; }
    else if ( SYM_LSET   == sym ) { return LSET; }
    else if ( SYM_FSET   == sym ) { return FSET; }
    else if ( SYM_CONTI  == sym ) { return CONTI; }
    else if ( SYM_NUATE  == sym ) { return NUATE; }
    else if ( SYM_FRAME  == sym ) { return FRAME; }
    else if ( SYM_PUSH   == sym ) { return PUSH; }
    else if ( SYM_SHIFT  == sym ) { return SHIFT; }
    else if ( SYM_CALL   == sym ) { return CALL; }
    else if ( SYM_GSET   == sym ) { return GSET; }
    else if ( SYM_RETURN == sym ) { return RETURN; }
    else if ( SYM_JUMP   == sym ) { return JUMP; }
    else { return UNKNOWN; }
#else
    if      ( SYM_NONE   == sym ) { return I_NONE; }
    else if ( SYM_HALT   == sym ) { return I_HALT; }
    else if ( SYM_LREF   == sym ) { return I_LREF; }
    else if ( SYM_FREF   == sym ) { return I_FREF; }
    else if ( SYM_GREF   == sym ) { return I_GREF; }
    else if ( SYM_UNBOX  == sym ) { return I_UNBOX; }
    else if ( SYM_CONST  == sym ) { return I_CONST; }
    else if ( SYM_CLOSE0 == sym ) { return I_CLOSE0; }
    else if ( SYM_CLOSE1 == sym ) { return I_CLOSE1; }
    else if ( SYM_CLOSE2 == sym ) { return I_CLOSE2; }
    else if ( SYM_BOX    == sym ) { return I_BOX; }
    else if ( SYM_TEST   == sym ) { return I_TEST; }
    else if ( SYM_LSET   == sym ) { return I_LSET; }
    else if ( SYM_FSET   == sym ) { return I_FSET; }
    else if ( SYM_CONTI  == sym ) { return I_CONTI; }
    else if ( SYM_NUATE  == sym ) { return I_NUATE; }
    else if ( SYM_FRAME  == sym ) { return I_FRAME; }
    else if ( SYM_PUSH   == sym ) { return I_PUSH; }
    else if ( SYM_SHIFT  == sym ) { return I_SHIFT; }
    else if ( SYM_CALL   == sym ) { return I_CALL; }
    else if ( SYM_GSET   == sym ) { return I_GSET; }
    else if ( SYM_RETURN == sym ) { return I_RETURN; }
    else if ( SYM_JUMP   == sym ) { return I_JUMP; }
    else { return I_UNKNOWN; }
#endif
}

SchObj to_insn_vec(SchObj lst)
{
    int    len = list_length(lst);
    SchObj vec = SCH_VECTOR(len);
    SchObj cdr = lst;
    int    i   = 0;
    int    ins;

    while ( i < len ) {

        ins = ins_val(SCH_CAR(cdr));
        SCH_VECTOR_REF(vec,i) = ins;
        cdr = SCH_CDR(cdr);
        ++i;

        /* operand 0 */
        if ( ins == NONE  || ins == PUSH || ins == UNBOX || ins == HALT || ins == CONTI ) {
            continue;
        }
        /* operand 1 */
        else if ( ins == LREF || ins == FREF   || ins == GREF || ins == TEST || ins == FRAME ||
                  ins == CALL || ins == RETURN || ins == BOX  || ins == LSET || ins == FSET  ||
                  ins == GSET || ins == JUMP   || ins == NUATE ) {
            SCH_VECTOR_REF(vec,i) = FIX2INT(SCH_CAR(cdr));
            cdr = SCH_CDR(cdr);
            ++i;
            continue;
        }
        else if ( ins == CONST ) {
            SCH_VECTOR_REF(vec,i) = SCH_CAR(cdr);
            cdr = SCH_CDR(cdr);
            ++i;
            continue;
        }

        /* operand 2 */
        else if ( ins == SHIFT ) {
            SCH_VECTOR_REF(vec,i) = FIX2INT(SCH_CAR(cdr));
            cdr = SCH_CDR(cdr);
            ++i;
            SCH_VECTOR_REF(vec,i) = FIX2INT(SCH_CAR(cdr));
            cdr = SCH_CDR(cdr);
            ++i;
            continue;
        }
        /* operand 3 */
        else if ( ins == CLOSE0 || ins == CLOSE1 ||ins == CLOSE2 ) {
            SCH_VECTOR_REF(vec,i) = FIX2INT(SCH_CAR(cdr));
            cdr = SCH_CDR(cdr);
            ++i;
            SCH_VECTOR_REF(vec,i) = FIX2INT(SCH_CAR(cdr));
            cdr = SCH_CDR(cdr);
            ++i;
            SCH_VECTOR_REF(vec,i) = FIX2INT(SCH_CAR(cdr));
            cdr = SCH_CDR(cdr);
            ++i;
            continue;
        }
    }
    return vec;
}

SchObj vm_eval(SchObj* code)
{
    SubrPnt pnt[NUM_OF_SUBR];
    char*   name[NUM_OF_SUBR];
    DisplayClosure* c = make_subrs(pnt,name);
    init_instructions();
    return vm(0,code,c,-1,0,0);
    SCH_FREE(c);                    /* TODO never reach here */
}

/* 名前がおかしい */
SchObj vm_compile(SchObj code)
{
    SubrPnt pnt[NUM_OF_SUBR];
    char*   name[NUM_OF_SUBR];
    DisplayClosure* c = make_subrs(pnt,name);
    SchObj icode;
    SchObj ivec;
    SchObj ret;

    init_instructions();
    init_globals();

    SchObj codes[] = {
        FRAME, 9,
          CONST, code, PUSH,
          GREF,   INDEX_P1,
          CALL,   1,
          RETURN, 1,
        HALT,
    };

    /* compile */
    if ( flag_use_profiler ) {
        start_profiler();
    }

    icode = vm(0,codes,c,-1,0,0);

/*     SCH_WRITE(icode); */
/*     printf("\n"); */
/*     do_dump = 1; */

    if ( flag_use_profiler ){
        stop_profiler();
/*         show_prof("compile time"); */
        show_prof_sorted("compile time");
    }

    ivec  = to_insn_vec(icode);

    /* execute */
    if ( flag_use_profiler ) {
        start_profiler();
    }
    ret = vm(0,SCH_VECTOR_OBJ(ivec)->vec,c,-1,0,0);
/*     SCH_WRITE(ret); */

    if ( flag_use_profiler ){
        stop_profiler();
        show_prof_sorted("run time");
    }

/*     printf("\n"); */

    SCH_FREE(c);
    return ret;
}


