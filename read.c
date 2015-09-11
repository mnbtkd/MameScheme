/* --------------------------------------------------------------------
 *   read.c -  MameScheme ( Scheme Interpreter based on R5RS )
 * --------------------------------------------------------------------
 *
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


#include <stdio.h>
#include "mame.h"

enum CH_CLASS {
    CL_WHITESPACE    =  1,
    CL_IDENTIFIER    =  2,
    CL_DECIMALNUMBER =  4,
    CL_LIST_BEGIN    =  8,
    CL_LIST_END      = 16,
    CL_DELIMITER     = 32
};

static char ch_class_[128];

int ch_class(unsigned int c) {
    return ch_class_[c];
}

void init_reader( void ) {

    int i;
    for ( i = 0; i < 128; i++ ) {
        ch_class_[i] = CL_IDENTIFIER;
    }

    for ( i = '0' ; i <= '9' ; ++i ) {
        ch_class_[i] = CL_DECIMALNUMBER;
    }

    ch_class_['\t']   = CL_WHITESPACE;
    ch_class_['\n']   = CL_WHITESPACE;
    ch_class_['\013'] = CL_WHITESPACE;
    ch_class_['\f']   = CL_WHITESPACE;
    ch_class_['\r']   = CL_WHITESPACE;
    ch_class_['\034'] = CL_WHITESPACE;
    ch_class_['\035'] = CL_WHITESPACE;
    ch_class_['\036'] = CL_WHITESPACE;
    ch_class_['\037'] = CL_WHITESPACE;
    ch_class_[' ']    = CL_WHITESPACE;

    ch_class_['('] =  CL_LIST_BEGIN;
    ch_class_[')'] =  CL_LIST_END;
    ch_class_['['] =  CL_LIST_BEGIN;
    ch_class_[']'] =  CL_LIST_END;

    ch_class_['"'] = CL_DELIMITER;
    ch_class_[';'] = CL_DELIMITER;
}

/* STRING UTIL */
#define STRLEN(str)             strlen(str)

/* BUFFER */
#define SCH_BUF_SIZE 512
static char token_buffer_[SCH_BUF_SIZE];
#define CLEAR_BUF(buf)          (buf[0] = '\0')
#define PUSH_BUF(buf,ch)                \
    do {                                \
        char* s = buf;                  \
        while (*s) {                    \
            ++s;                        \
        }                               \
        *s = ch;                        \
        s++;                            \
        *s = '\0';                      \
    } while(0)
#define POP_BUF(buf)                            \
    do {                                        \
        char* s = buf;                          \
        while(*s) {                             \
            ++s;                                \
        }                                       \
        *(--s) = '\0';                          \
    } while(0)
#define TO_S_BUF(buf)           buf
#define BUF_SIZE(buf)           STRLEN(buf)



SchObj read_number( char* s, int radix )
{
    char* str    = s;
    char* endstr = NULL;
    long n;

    if ( STRLEN(str) < 1 ) {
        return SCH_FALSE;       /* TODO throw exception */
    }

    n = strtol(str, &endstr, radix);

    if ( *endstr == '\0' ) {    /* FIXNUM or BIGNUM */
        if (FIXABLE(n)) {
            return INT2FIX(n);
        } else {
            return str2bignum(str,radix);
        }
    } else if ( *endstr == '/' && (str != endstr) ) { /* RATIONAL */

        SchObj nobj,lobj;

        if ( FIXABLE(n) ) {
            nobj = INT2FIX(n);
        } else {
            size_t size = endstr - str;
            char * tmp = malloc(size+1);

            if ( tmp ) {
                memcpy(tmp,str,size);
                tmp[size] = '\0';
                nobj = str2bignum(tmp,10);
                free(tmp);
            } else {
                EXCEPTION("no more free space.");
            }
        }

        str = endstr+1;
        long l = strtol(str, &endstr, radix);

        if ( *endstr == '\0' ) {
            lobj = (FIXABLE(n) ? INT2FIX(l) : str2bignum(str,10)) ;
            return SCH_RATIONAL(nobj,lobj);
        } else {
            return SCH_FALSE;
        }

    } else {                    /* FLOAT */
        double d = strtod(str,&endstr);
        double d0,d1;

        if ( *endstr == '\0' ) {
            d0 = ceil(d);
            d1 = floor(d);
            /* TODO kludge */
            if ( d0 == d1 && d1 == d) {
                return INT2FIX((int)d);
            }
            return SCH_FLOAT(d);
        } else {
            EXCEPTION("failed to convert to both integer and float.");
            return SCH_FALSE;
        }
    }
}

char* token( SchPort* port ) {

    unsigned int c;
    int cls;

    CLEAR_BUF(token_buffer_);

    while( (c = SCH_GETC(port)) != EOF ) {
        cls = ch_class(c);
        if ( cls & CL_WHITESPACE ) {
            break;
        } else if ( cls & (CL_LIST_BEGIN|CL_LIST_END|CL_DELIMITER) ) {
            SCH_UNGETC(c,port);
            break;
        } else {
            PUSH_BUF(token_buffer_,c);
        }
    }
    return token_buffer_;
}

SchObj read_obj( SchPort* port );

SchObj read_sharp_sequence( SchPort* port ) {

    unsigned int c = SCH_GETC(port);
    char *s, *format;
    char msg[256];              /* TODO hard code */
    int  size;

    switch(c) {
    case 't':
        s = token(port);
        if ( STRLEN(s) != 0 ) {
            format="#t followed by garbage \"%s\"";
            sprintf(msg,format,s);
            EXCEPTION(msg);
            break;
        } else {
            return SCH_TRUE;
        }
    case 'f':
        s = token(port);
        if ( STRLEN(s) != 0 ) {
            format="#f followed by garbage \"%s\"";
            sprintf(msg,format,s);
            EXCEPTION(msg);
            break;
        } else {
            return SCH_FALSE;
        }
    case '\\':
        c    = SCH_GETC(port);
        size = get_char_size(c);
        if ( size > 1 ) {
            unsigned int c4 = (unsigned char)c;
            while ( --size > 0 && (c = SCH_GETC(port)) ) {
                c4 = ((c4 << 8) | (unsigned char)c);
            }
            return make_char(c4);
        } else {
            s = token(port);
            if ( strlen(s) == 0 ) {
                return make_char(c);
            } else if (c == 's') {
                if ( strcmp(s,"pace")    == 0 ) { return make_char(' ');  }
            } else if (c == 'n') {
                if ( strcmp(s,"ewline")  == 0 ) { return make_char('\n'); }
            } else if (c == 't') {
                if ( strcmp(s,"ab")      == 0 ) { return make_char('\t'); }
            } else if (c == 'f') {
                if ( strcmp(s,"ormfeed") == 0 ) { return make_char('\f'); }
            } else if (c == 'r') {
                if ( strcmp(s,"eturn")   == 0 ) { return make_char('\r'); }
            } else {
                char msg[256];
                char* format="unknown character #\\%c%s";
                sprintf(msg,format,c,s);
                EXCEPTION(msg);
            }
        }
        break;
    case '(': {
        SchObj x, vec;
        size = 0;
        SchVecLList* lst = make_vec_llist();
        while ( (x = read_obj(port)) != SCH_KOKKA ) {
            vec_push(lst,x);
            size++;
        }
        vec = (SchObj)make_vec_from_list(lst,size);
        return vec;
    }
    case 'b':
        s = token(port);
        return read_number(s, 2);
    case 'o':
        s = token(port);
        return read_number(s, 8);
    case 'd':
        s = token(port);
        return read_number(s, 10);
    case 'x':
        s = token(port);
        return read_number(s, 16);
    default:
        format = "unknown syntax #%c";
        sprintf(msg,format,c);
        EXCEPTION(msg);
    }

    return SCH_UNDEFINE;
}

SchObj read_obj( SchPort* port ) {
    unsigned int c;

    do {
        c = SCH_GETC(port);
    } while ( ch_class(c) == CL_WHITESPACE );

    if ( c == ((unsigned char)EOF) ) { /* c -> 0x 00 00 00 ff,  EOF -> 0x ff */
        return SCH_EOF;
    }

    CLEAR_BUF(token_buffer_);

    if ( ch_class(c) == CL_DECIMALNUMBER || c == '-' || c == '+') {

        unsigned int c0 = c;

        PUSH_BUF(token_buffer_,c0);
        c = SCH_GETC(port);

        if ( (c0 == '-' || c0 == '+') && (ch_class(c) != CL_DECIMALNUMBER) ) {
            SCH_UNGETC(c,port);
            return SCH_SYMBOL(TO_S_BUF(token_buffer_));
        }

        while ( ch_class(c) & (CL_DECIMALNUMBER) || c == '.' || c == '/' ) {
            PUSH_BUF(token_buffer_,c);
            c = SCH_GETC(port);
        }

        if ( c != '\0' ) {
            SCH_UNGETC(c,port);
        }

        return read_number(TO_S_BUF(token_buffer_),10);

    } else if ( ch_class(c) == CL_LIST_END ) {

        return SCH_KOKKA;

    } else if ( ch_class(c) == CL_LIST_BEGIN ) {

        SchObj x      = read_obj(port);
        SchObj p_val  = SCH_LIST1(x);
        SchObj p_last = p_val;

        if ( x == SCH_KOKKA ) {
            return SCH_NIL;
        }

        for (;;) {
            x = read_obj( port );
            if ( x == SCH_KOKKA ) {
                return p_val;
            } else if ( x == SCH_DOT || x == SCH_SYMBOL(".") ) {
                SCH_SET_CDR( p_last, read_obj(port) );
                if ( read_obj( port ) != SCH_KOKKA ) {
                    EXCEPTION("right parenthesis ')' missing");
                }
                return p_val;
            } else {
                SchObj p_y = SCH_LIST1(x);
                SCH_SET_CDR(p_last,p_y);
                p_last = p_y;
            }
        }

    } else if ( c == '"' ) {

        unsigned int prev = c;
        while ( (c = SCH_GETC(port)) != '"' || prev == '\\') {
            if ( prev == '\\' ) {
                switch(c) {
                case '"':
                    POP_BUF(token_buffer_);
                    PUSH_BUF(token_buffer_,'\"');
                    break;
                case 'n':
                    POP_BUF(token_buffer_);
                    PUSH_BUF(token_buffer_,'\n');
                    break;
                case 't':
                    POP_BUF(token_buffer_);
                    PUSH_BUF(token_buffer_,'\t');
                    break;
                default:
                    break;
                }
            } else {
                PUSH_BUF(token_buffer_,c);
            }
            prev = c;
        }
        return SCH_STRING(TO_S_BUF(token_buffer_));

    } else if ( c == '\'' ) {

        return SCH_LIST2(SCH_SYMBOL("quote"),read_obj(port));

    } else if ( c == '`' ) {

        return SCH_LIST2( SCH_SYMBOL("quasiquote"), read_obj(port) );

    } else if ( c == ',' ) {

        if ( (c = SCH_GETC(port)) == '@' ) {
            return SCH_LIST2( SCH_SYMBOL("unquote-splicing"), read_obj(port) );
        } else {
            SCH_UNGETC(c,port);
            return SCH_LIST2( SCH_SYMBOL("unquote"), read_obj(port) );
        }

    } else if ( c == ';' ) {

        while ( (c = SCH_GETC(port)) != '\n' ) {
            if (c == (unsigned char)EOF) { return (SchObj)SCH_EOF; }
        }
        return read_obj( port );

    } else if ( c == '#') {
        return read_sharp_sequence(port);
    } else {

        do {
            PUSH_BUF(token_buffer_,c);
            c = SCH_GETC(port);
        } while ( ch_class(c) & (CL_IDENTIFIER|CL_DECIMALNUMBER) );

        SCH_UNGETC(c,port);

        return SCH_SYMBOL(TO_S_BUF(token_buffer_));
    }

}

SchObj sch_read_string( char* str ) {
    SchPort* port;
    port = (SchPort*)make_input_port_string( str );
    return read_obj( port );
}

SchObj read_obj_via_port( SchPort* port ) {
    return read_obj(port);
}

void show_prompt()
{
    printf("\nmame> ");
}

SchObj interact()
{
    SchPort* port;
    show_prompt();
    port = (SchPort*)make_input_port_stream(stdin);
    return read_obj( port );
}

SchObj sch_read ( FILE* fp )
{
    if ( fp ) {
        return 0;
    } else {
        return interact();
    }
}

