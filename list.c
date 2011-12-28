/*--------------------------------------------------------------------
  list.c -  MameScheme ( Scheme Interpreter based on R5RS )
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
#include <assert.h>

SchObj make_pair(SchObj car, SchObj cdr )
{
    SchPair* ptr;
    ptr = SCH_MALLOC( sizeof(SchPair) );
    ptr->hd.flags = T_PAIR;
    SCH_CAR(ptr) = car;
    SCH_CDR(ptr) = cdr;
    return (SchObj)ptr;
}

int list_length( SchObj list )
{
    SchObj x;
    int c;
    if ( ! PAIRP(list) ) {
        EXCEPTION("a list required");
    }
    if ( NULLP(list) ) {
        return 0;
    }
    c = 1;
    x = SCH_CDR(list);
    while ( ! NULLP(x) ) {
        ++c;
        x = SCH_CDR(x);
    }
    return c;
}

int is_list(SchObj list)
{
    SchObj x = list;

    if ( ! PAIRP(x) ) {
        return 0;
    }

    while ( 1 ) {               /* TODO if list is circular , never return.*/
        x = SCH_CDR(x);
        if ( ! PAIRP(x) ) {
            return NULLP(x) ? 1 : 0;
        }
    }
}

SchObj last_cons(SchObj list)
{
    SchObj x = list;
    SchObj prev;

    if ( ! PAIRP(x) ) {
        EXCEPTION("pair required");
    } else {

        while ( 1 ) {               /* TODO if list is circular , never return.*/
            prev = x;
            x = SCH_CDR(x);
            if ( ! PAIRP(x) ) {
                return prev;
            }
        }
    }
    assert(1);
    return SCH_NIL;
}

SchObj list_ref(SchObj list, int index)
{
    SchObj x = list;
    int i;
    for ( i = 0 ; i<index ; ++i ) {
        if ( ! PAIRP(x) ) {
            EXCEPTION("out of index in list_ref");
            return SCH_NIL;
        }
        x = SCH_CDR(x);
    }

    if ( PAIRP(x) ) {
        return SCH_CAR(x);
    } else {
        EXCEPTION("error at list_ref");
        return SCH_NIL;
    }
}

SchObj list_tail(SchObj list, int index)
{
    SchObj x = list;
    int i;
    for ( i = 0 ; i<index ; ++i ) {
        if ( ! PAIRP(x) ) {
            EXCEPTION("out of index in list_tail");
            return SCH_NIL;
        }
        x = SCH_CDR(x);
    }
    return x;
}

SchObj reverse(SchObj lst)
{
    SchObj x = lst;
    SchObj tail = SCH_NIL;

    if ( ! PAIRP(x) ) {
        return lst;
    }

    while ( PAIRP(x)) {
        tail = SCH_CONS(SCH_CAR(x),tail);
        x = SCH_CDR(x);
    }
    return tail;
}

SchObj list2vector(SchObj lst)
{
    SchObj vec;
    size_t size;
    SchObj x = lst;
    int i;

    if ( ! PAIRP(x) ) {
        EXCEPTION("a list required");
        return SCH_NIL;
    }

    size = list_length(x);
    vec  = SCH_VECTOR(size);

    for ( i = 0 ; i < size ; ++i ) {
        SCH_VECTOR_REF(vec,i) = SCH_CAR(x);
        x = SCH_CDR(x);
    }

    return vec;
}
