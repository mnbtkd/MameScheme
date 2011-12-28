/*--------------------------------------------------------------------
  vector.c -  MameScheme ( Scheme Interpreter based on R5RS )
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



SchObj make_vector(int size)
{
    SchVector* ptr;
    int i;
    ptr = SCH_MALLOC(sizeof(SchVector));
    ptr->hd.flags = T_VECTOR;
    ptr->vec = SCH_MALLOC(sizeof(SchObj)*size);
    ptr->size = size;
    for ( i=0 ; i<size ; ++i ) {
        ptr->vec[i] = SCH_UNDEFINE;
    }

    return (SchObj)ptr;
}

SchObj make_vector_fill(int size, SchObj fill)
{
    SchVector* vec;
    int i;
    vec = SCH_VECTOR_OBJ(make_vector(size));
    for ( i=0 ; i<size ; ++i ) {
        vec->vec[i] = fill;
    }
    return (SchObj)vec;
}

SchObj vector_fill(SchObj vec, SchObj obj)
{
    size_t i,len;

    if ( ! VECTORP(vec) ) {
        EXCEPTION("a vector required");
        return SCH_NIL;
    }

    len = SCH_VECTOR_LEN(vec);
    for ( i = 0 ; i < len ; ++i ) {
        SCH_VECTOR_REF(vec,i) = obj;
    }
    return SCH_UNDEFINE;
}

SchObj vector2list(SchObj vec)
{
    SchObj lst = SCH_NIL;
    size_t len;

    if ( !VECTORP(vec) ) {
        EXCEPTION("a vector required");
        return SCH_NIL;
    }

    len = SCH_VECTOR_LEN(vec);
    do {
        len--;
        lst = SCH_CONS(SCH_VECTOR_REF(vec,len),lst);
    } while ( len > 0);
    return lst;
}

SchVecLList* make_vec_llist(void)
{
    SchVecLList* lst;
    lst = SCH_MALLOC(sizeof(SchVecLList));
    lst->obj = 0;
    lst->next = NULL;
    return lst;
}

void vec_push(SchVecLList* lst, SchObj x)
{
    SchVecLList* newl;
    newl = make_vec_llist();
    newl->obj = x;
    newl->next = lst->next;
    lst->next = newl;
}


SchVector* make_vec_from_list(SchVecLList* lst, int size)
{
    SchVecLList* tmpl;
    SchVecLList* next;
    SchVector* vec;
    vec = SCH_VECTOR_OBJ(make_vector(size));
    tmpl = lst->next;

    for ( --size ; size>=0 ; --size ) {
        vec->vec[size] = tmpl->obj;
        next = tmpl->next;
        SCH_FREE(tmpl);
        tmpl = next;
    }
    SCH_FREE(lst);

    return vec;
}

