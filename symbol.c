/*--------------------------------------------------------------------
  symbol.c -  MameScheme ( Scheme Interpreter based on R5RS )
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
#include "st.h"

static int last_id=0;
static st_table* sym_table=0;
static st_table* rev_table=0;

#define NEW_ID()   (((last_id++)<<2) + T_SYMBOL);

static unsigned long ids[1024*16];
static int size=0;

void ensure_init_symtable(void)
{
    if ( ! sym_table ) {
        sym_table = st_init_strtable();
    }
    if ( ! rev_table ) {
        rev_table = st_init_numtable();
    }
}

void sch_sym_insert( char* str, SchObj id )
{
    char* name = SCH_MALLOC(1024);
    strncpy(name,str,1023);

    st_insert(sym_table,name,id);
    st_insert(rev_table,id,name);
}

SchObj sch_intern( char* str )
{
    SchObj id;
    if ( ! st_lookup(sym_table, str, &id) ) {
        id = NEW_ID();
        sch_sym_insert(str, id);
        ids[size++] = id ;
    }

    return id;
}

char* sch_id2name( SchObj id )
{
    char* ret;
    if ( ! st_lookup(rev_table, id, &ret ) ) {
        ret="";
    }
    return ret;
}

char* symsd(void)
{
    int i;
    char* name;
    char* buf = SCH_MALLOC(1024);
    char tmp[256];
    tmp[0]='\0';
    buf[0] = '\n';

    for ( i=0 ; i<size ; ++i ) {
        name = sch_id2name(ids[i]);
        sprintf(tmp,"[%#010lx](%s)\n",ids[i],name);
        strcat(buf,tmp);
    }
    return buf;
}
