/*--------------------------------------------------------------------
  port.c -  MameScheme ( Scheme Interpreter based on R5RS )
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
#include <stdio.h>

#ifdef ENCODING_UTF8
/* utf-8 version */
int get_char_size ( unsigned char c )
{
    if ( (c & 0x80) == 0) return 1;
    if ( (c & 0x20) == 0) return 2;
    if ( (c & 0x10) == 0) return 3;
    if ( (c & 0x08) == 0) return 4;
    if ( (c & 0x04) == 0) return 5;
    if ( (c & 0x02) == 0) return 6;
    return -1;
}
#endif
#ifdef ENCODING_SJIS
/* sjis version */
int get_char_size ( unsigned char c )
{
    if ( ((c)>=0x81 && (c)<=0x9f) || ((c)>=0xe0 && (c)<=0xef) ) {
        return 2;
    } else {
        return 1;
    }
}
#endif

/* TODO boundary check */
unsigned int stream_getc( SchPort* port )
{
    unsigned char c;
    unsigned int ch;
    int  size;
    ch = c = (unsigned char)fgetc(port->src.stream.fp);
    size = get_char_size(c);
    while ( size > 1 ) {
        c = fgetc(port->src.stream.fp);
        ch = (ch << 8U) | c;
        --size;
    }
    return ch;
}
char stream_getbyte( SchPort* port )
{
    return (char)fgetc(port->src.stream.fp);
}
void stream_ungetc( char c, SchPort* port )
{
    ungetc(c, port->src.stream.fp);
}
char string_getbyte( SchPort* port )
{
    char ret;
    char* current;
    int index;
    current = port->src.string.current;
    index   = port->src.string.index;

    if ( *current == '\0' && index >= 0 ) {
        return SCH_EOF;
    }

    ret = *(++(port->src.string.current));
    (port->src.string.index)++;
    return (( ret == '\0') ? EOF : ret );
}
unsigned int string_getc( SchPort* port )
{
    unsigned char  c;
    char* current;
    int   index;
    unsigned int ch;

    current = port->src.string.current;
    index   = port->src.string.index;

    if ( *current == '\0' && index >= 0 ) {
        return SCH_EOF;
    }

    ch = c = *(++(port->src.string.current));
/*     ch = 0U | ((unsigned char)c); */
    int size = get_char_size(c);

    (port->src.string.index) += size;
    while ( size > 1 ) {
        c  = *(++(port->src.string.current));
        ch = (ch << 8U) | ((unsigned char)c);
        --size;
    }

    return (( ch == 0U ) ? EOF : ch );
}
void string_ungetc( char c, SchPort* port )
{
    char* current;
    char* string;
    current = port->src.string.current;
    string  = port->src.string.str;
    if ( current < string ) {
    } else {
        (port->src.string.current)--;
        (port->src.string.index)--;
    }
}

SchPort* make_input_port_string( char* str )
{
    SchPort* port;
    port = SCH_MALLOC( sizeof(SchPort) );

    port->hd.flags  = T_PORT;
    port->direction = SCH_PORT_INPUT;
    port->type      = SCH_PORT_STRING;
    port->line      = 0;

    port->src.string.str     = str;
    port->src.string.current = str-1;
    port->src.string.index   = -1;

    port->sch_getc    = string_getc;
    port->sch_getbyte = string_getbyte;
    port->sch_ungetc  = string_ungetc;

    return port;
}

SchPort* make_output_port_string( char* str )
{
    SchPort* port;
    port = SCH_MALLOC( sizeof(SchPort) );

    port->hd.flags  = T_PORT;
    port->direction = SCH_PORT_OUTPUT;
    port->type      = SCH_PORT_STRING;
    port->line      = 0;

    port->src.string.str     = str;
    port->src.string.current = str-1;
    port->src.string.index   = -1;

    port->sch_getc    = string_getc;
    port->sch_getbyte = string_getbyte;
    port->sch_ungetc  = string_ungetc;

    return port;
}

SchPort* make_input_port_stream(FILE* fp )
{
    SchPort* port;
    port = SCH_MALLOC( sizeof(SchPort) );

    port->hd.flags  = T_PORT;
    port->direction = SCH_PORT_INPUT;
    port->type      = SCH_PORT_STREAM;
    port->line      = 0;

    port->src.stream.status = SCH_PORT_OPEN;
    port->src.stream.fp     = fp;

    port->sch_getc    = stream_getc;
    port->sch_getbyte = stream_getbyte;
    port->sch_ungetc  = stream_ungetc;

    return port;
}

SchPort* make_output_port_stream(FILE* fp )
{
    SchPort* port;
    port = SCH_MALLOC( sizeof(SchPort) );

    port->hd.flags  = T_PORT;
    port->direction = SCH_PORT_OUTPUT;
    port->type      = SCH_PORT_STREAM;
    port->line      = 0;

    port->src.stream.status = SCH_PORT_OPEN;
    port->src.stream.fp     = fp;

    port->sch_getc    = stream_getc;
    port->sch_getbyte = stream_getbyte;
    port->sch_ungetc  = stream_ungetc;

    return port;
}

unsigned int read_char( SchPort* port )
{
    unsigned int ch = SCH_GETC(port);
    return ch;
}
char peek_char( SchPort* port ){return 0;}


BOOL is_input_port( SchPort* port )
{
    return ( port->direction == SCH_PORT_INPUT );
}

BOOL is_output_port( SchPort* port )
{
    return ( port->direction == SCH_PORT_OUTPUT );
}

SchObj open_input_file( char* filename )
{
    FILE* fp;
    if ( (fp = fopen(filename,"r")) == NULL ) {
        EXCEPTION("file cannot open");
    }
    return (SchObj)make_input_port_stream(fp);
}
SchObj open_output_file( char* filename )
{
    FILE* fp;
    if ( (fp = fopen(filename,"w")) == NULL ) {
        EXCEPTION("file cannot open");
    }
    return (SchObj)make_output_port_stream(fp);
}

void close_port( SchPort* port)
{
    if ( SCH_PORT_OBJ(port)->type == SCH_PORT_STREAM ) {
        if ( EOF == fclose(SCH_PORT_OBJ(port)->src.stream.fp) ) {
            EXCEPTION("stream close failed");
        } else {
            SCH_PORT_OBJ(port)->src.stream.status = SCH_PORT_CLOSED;
        }
    }
}


SchPort* sch_stdout;
SchPort* sch_stdin;
static SchPort* curr_iport;
static SchPort* curr_oport;

SchPort* current_input_port(void)
{
    return curr_iport;
}

SchPort* current_output_port(void)
{
    return curr_oport;
}

void set_current_input_port(SchPort* port)
{
    curr_iport = port;
}

void set_current_output_port(SchPort* port)
{
    curr_oport = port;
}

void init_port(void)
{
    curr_iport = sch_stdin  = make_input_port_stream(stdin);
    curr_oport = sch_stdout = make_output_port_stream(stdout);
}

