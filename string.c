/*--------------------------------------------------------------------
  string.c -  MameScheme ( Scheme Interpreter based on R5RS )
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


int mbslen(char* str)
{
    int size,len;
    unsigned char c=0;
    len=0;

    while ( *str ) {
        c = (unsigned char)*str;
        size = get_char_size(c);
        str += size;
        len++;
    }
    return len;
}

#ifdef ENCODING_SJIS
  #define MAXCHARSIZE    2
#endif

#ifdef ENCODING_UTF8
  #define MAXCHARSIZE    4
#endif

#if defined ENCODING_SJIS
  #define STR_LEN(s)      (mbslen(s))
#elif defined ENCODING_UTF8
  #define STR_LEN(s)      (mbslen(s))
#else
  #define STR_LEN(s)      (strlen(s))
#endif

#define STR_SIZE(s)     (strlen(s))

SchObj make_char( unsigned int c )
{
    SchChar* ptr;
    ptr = SCH_MALLOC( sizeof(SchChar) );
    ptr->hd.flags = T_CHAR;
    ptr->ch = c;
    return (SchObj)ptr;
}

int get_char_byte_size(unsigned int ch)
{
    if ((ch & 0xff000000U) != 0) return 4;
    if ((ch & 0xff0000U)   != 0) return 3;
    if ((ch & 0xff00U)     != 0) return 2;
    if ((ch & 0xffU)       != 0) return 1;

    return 0;
}

SchObj subr_is_eq_char( SchObj args )
{
    return CHAR_EQUALP(SCH_CAR(args),
                       SCH_CADR(args));
}

SchObj make_string( char* s )
{
    SchString* ptr;
    ptr           = SCH_MALLOC( sizeof(SchString) );
    ptr->hd.flags = T_STRING;
    ptr->size     = STR_SIZE( s );
    ptr->len      = STR_LEN( s );
    ptr->buf      = SCH_MALLOC( ptr->size + 1 );
    strcpy( ptr->buf, s );

    return (SchObj)ptr;
}

SchObj make_string_k( int k )
{
    SchString* ptr;
    ptr           = SCH_MALLOC( sizeof(SchString) );
    ptr->hd.flags = T_STRING;
    ptr->len      = k;
    ptr->size     = (k+1)*MAXCHARSIZE;
    ptr->buf      = SCH_MALLOC(ptr->size);
    ptr->buf[0]   = '\0';

    return (SchObj)ptr;
}

SchObj string_ref( SchString* str, int i )
{
    return SCH_CHAR( str->buf[i] );
}

void string_set( SchString* str, int i, char c)
{
    str->buf[i] = c;
}

SchObj make_string_k_ch(int k, char c)
{
    SchString* ptr;
    int i;
    ptr = SCH_STRING_OBJ( make_string_k(k) );
    for ( i=0 ; i<k ; ++i ) {
        string_set( ptr, i, c );
    }
    return (SchObj)ptr;
}

char* set_forward(char* str, int size)
{
    while ( size > 0 ) {
        str += get_char_size(*str);
        --size;
    }
    return str;
}

SchObj substring( SchObj str,
                  int    beg,
                  int    end )
{
    SchObj ret;
    size_t len;
    int    diff;
    int    size;
    char* s;
    char *buf;

    if ( ! STRINGP(str) ) {
        EXCEPTION("string required");
    }

    len = SCH_STRING_OBJ(str)->len;

    if ( len < beg || len < end || beg > end) {
        EXCEPTION("out of range");
    }

    ret  = make_string_k( end - beg );
    diff = end - beg;

    s   = SCH_STRING_OBJ(str)->buf;
    buf = SCH_STRING_OBJ(ret)->buf;

    s = set_forward(s,beg);

    while ( diff > 0 ) {
        size = get_char_size(*s);
        while ( size > 0 ) {
            *buf = *s;
            buf++;
            s++;
            --size;
        }
        --diff;
    }

    return ret;
}

SchObj string2list( SchObj str )
{
    char *       buf = SCH_STRING_OBJ(str)->buf;
    unsigned int len = SCH_STRING_OBJ(str)->len;
    SchPort*     p   = make_input_port_string(buf);
    unsigned int ch;
    SchObj       ret;
    SchObj       last_cons;

    if ( len > 0 ) {
        ch = SCH_GETC(p);
        last_cons = ret = SCH_LIST1(SCH_CHAR(ch));
        --len;
        while ( len > 0 ) {
            ch = SCH_GETC(p);
            SCH_SET_CDR(last_cons,SCH_LIST1(SCH_CHAR(ch)));
            last_cons = SCH_CDR(last_cons);
            --len;
        }
        return ret;
    } else {
        return SCH_NIL;
    }
}

SchObj list2string( SchObj lst )
{
    SchObj kar,ret;
    unsigned int ch;
    SchObj SCH_MANGLE(tmp);

    ret = make_string_k(10);
    char* buf = SCH_STRING_OBJ(ret)->buf;

    FOR_EACH(kar,lst){
        ch   = SCH_CHAR_OBJ(kar)->ch;
        if ( ch ) {
            if ( ((ch >> 24) & 0xffU) != 0 ){
                *buf++ = (unsigned char)((ch >> 24) & 0xffU);
                SCH_STRING_OBJ(ret)->size++;
            }
            if ( ((ch >> 16) & 0xffU) != 0 ){
                *buf++ = (unsigned char)((ch >> 16) & 0xffU);
                SCH_STRING_OBJ(ret)->size++;
            }
            if ( ((ch >>  8) & 0xffU) != 0 ){
                *buf++ = (unsigned char)((ch >>  8) & 0xffU);
                SCH_STRING_OBJ(ret)->size++;
            }
            *buf++ = (unsigned char)(ch & 0xffU);
            SCH_STRING_OBJ(ret)->size++;
        }
        SCH_STRING_OBJ(ret)->len++;
    }

    *buf = '\0';
    return ret;
}

SchObj string_fill( SchObj str, SchObj chobj )
{
    char* buf     = SCH_STRING_OBJ(str)->buf;
    int   org_len = SCH_STRING_OBJ(str)->len;

    unsigned int ch = SCH_CHAR_OBJ(chobj)->ch;

    unsigned int ch_size,len,shift_bit,i,j;

    if        ( 0 != (ch >> 24) ) {
        ch_size = 4;
    } else if ( 0 != (ch >> 16) ) {
        ch_size = 3;
    } else if ( 0 != (ch >>  8) ) {
        ch_size = 2;
    } else {
        ch_size = 1;
    }

    len = (SCH_STRING_OBJ(str)->size) / ch_size;
    if ( len > org_len ) {
        len = org_len;
    }

    for ( i=0 ; i < len ; ++i ) {
        for ( j=0 ; j < ch_size ; ++j ) {
            shift_bit = ((ch_size-1)*8) - (j*8);
            *buf++ = (unsigned char)((ch >> shift_bit) & 0xffU);
        }
    }
    *buf = '\0';

    SCH_STRING_OBJ(str)->size = len*ch_size;
    SCH_STRING_OBJ(str)->len  = len;

    return str;
}
