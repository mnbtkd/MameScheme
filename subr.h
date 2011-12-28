/*--------------------------------------------------------------------
  subr.h -  MameScheme ( Scheme Interpreter based on R5RS )
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

#ifndef INCLUDE_SUBR_H
#define INCLUDE_SUBR_H


extern SchObj stack[];
#define INDEX_ST(__s__,__i__)           (stack[(__s__)-(__i__)-1])
#define INDEX_SET(__s__,__i__,__obj__)  (stack[(__s__)-(__i__)-1]=(__obj__))

#define POP_STACK(_s)            (INDEX_ST(_s--,0))
#define LOOP_STACK(_s,_n,_x)     for(;_n>0 && ((_x = INDEX_ST(_s, 0)),1);_s--,_n--)
#define LOOP_STACK_REV(_s,_n,_x) for(;_n>0 && ((_x = INDEX_ST(_s,_n-1)),1);     _n--)


typedef SchObj (*SubrPnt)(int,int);

typedef struct ClosureBodyRec {
    int      body_size;
    SchObj*  body_top;
} ClosureBody;

typedef struct DisplayClosureRec {
    SchHeader hd;
    int argtype;
    int argnum;
    char* name;
    union {
        ClosureBody body;
        SubrPnt subr;
    } u;
    SchObj * vars;
    int vars_size;
} DisplayClosure;

SchPort* sch_stdout;
SchPort* sch_stdin;

#define SCH_CLOSURE_OBJ(_x)   ((DisplayClosure*)_x)

extern SchObj subr_add(int s, int n);
extern SchObj subr_mul(int s, int n);
extern SchObj subr_sub(int s, int n);
extern SchObj subr_div(int s, int n);
extern SchObj subr_is_greater(int s, int n);
extern SchObj subr_is_less(int s, int n);
extern SchObj subr_is_zero(int s, int n);
extern SchObj subr_is_equal_to_or_greater(int s, int n);
extern SchObj subr_is_equal_to_or_less(int s, int n);
extern SchObj subr_is_equal_num(int s, int n);
extern SchObj subr_not(int s, int n);
extern SchObj subr_is_boolean(int s, int n);
extern SchObj subr_is_pair(int s, int n);
extern SchObj subr_cons(int s, int n);
extern SchObj subr_car(int s, int n);
extern SchObj subr_cdr(int s, int n);
extern SchObj subr_set_car(int s, int n);
extern SchObj subr_set_cdr(int s, int n);
extern SchObj subr_is_null(int s, int n);
extern SchObj subr_list(int s, int n);
extern SchObj subr_length(int s, int n);
extern SchObj subr_is_symbol(int s, int n);
extern SchObj subr_symbol2string(int s, int n);
extern SchObj subr_string2symbol(int s, int n);
extern SchObj subr_string2list(int s, int n);
extern SchObj subr_list2string(int s, int n);
extern SchObj subr_string_fill(int s, int n);

extern SchObj subr_is_char( int s, int n );
extern SchObj subr_char_is_equal( int s, int n );
extern SchObj subr_char_is_equal_ci( int s, int n );
extern SchObj subr_char_is_greater( int s, int n);
extern SchObj subr_char_is_greater_ci( int s, int n);
extern SchObj subr_char_is_less( int s, int n);
extern SchObj subr_char_is_less_ci( int s, int n);
extern SchObj subr_char_is_equal_to_or_greater( int s, int n);
extern SchObj subr_char_is_equal_to_or_greater_ci( int s, int n);
extern SchObj subr_char_is_equal_to_or_less( int s, int n);
extern SchObj subr_char_is_equal_to_or_less_ci( int s, int n);
extern SchObj subr_char2integer(int s, int n);
extern SchObj subr_integer2char(int s, int n);
extern SchObj subr_char_is_alphabetic(int s, int n);
extern SchObj subr_char_is_whitespace(int s, int n);
extern SchObj subr_char_is_lower_case(int s, int n);
extern SchObj subr_char_is_upper_case(int s, int n);
extern SchObj subr_char_is_numeric(int s, int n);
extern SchObj subr_char_upcase(int s, int n);
extern SchObj subr_char_downcase(int s, int n);

extern SchObj subr_is_string(int s, int n);
extern SchObj subr_string_length(int s, int n);
extern SchObj subr_make_string_k_ch(int s, int n);
extern SchObj subr_make_string_k(int s, int n);
extern SchObj subr_string(int s, int n);
extern SchObj subr_string_ref(int s, int n);
extern SchObj subr_string_set(int s, int n);

extern SchObj subr_is_vector(int s, int n);
extern SchObj subr_make_vector(int s, int n);
extern SchObj subr_vector_length(int s, int n);
extern SchObj subr_vector_ref(int s, int n);
extern SchObj subr_vector_set(int s, int n);

extern SchObj subr_number2string(int s, int n);
extern SchObj subr_number2string_with_rad(int s, int n);

extern SchObj subr_is_eq(int s, int n);
extern SchObj subr_is_eqv(int s, int n);
extern SchObj subr_is_equal(int s, int n);

extern SchObj subr_is_number(int s, int n);
extern SchObj subr_is_procedure(int s, int n);
extern SchObj subr_apply(int s, int n);

extern SchObj subr_is_list(int s, int n);

extern SchObj subr_append(int s, int n);
extern SchObj subr_memq(int s, int n);
extern SchObj subr_memv(int s, int n);
extern SchObj subr_assq(int s, int n);
extern SchObj subr_assv(int s, int n);
extern SchObj subr_assoc(int s, int n);

extern SchObj subr_caar(int s, int n);
extern SchObj subr_cadr(int s, int n);
extern SchObj subr_cdar(int s, int n);
extern SchObj subr_cddr(int s, int n);

extern SchObj subr_cadar(int s, int n);
extern SchObj subr_caddr(int s, int n);
extern SchObj subr_cdddr(int s, int n);
extern SchObj subr_cadddr(int s, int n);
extern SchObj subr_cddddr(int s, int n);
extern SchObj subr_caddddr(int s, int n);

extern SchObj subr_caaaar(int s, int n);
extern SchObj subr_caaadr(int s, int n);
extern SchObj subr_caaar(int s, int n);
extern SchObj subr_caadar(int s, int n);
extern SchObj subr_caaddr(int s, int n);
extern SchObj subr_caadr(int s, int n);
extern SchObj subr_cadaar(int s, int n);
extern SchObj subr_cadadr(int s, int n);
extern SchObj subr_caddar(int s, int n);
extern SchObj subr_cdaaar(int s, int n);
extern SchObj subr_cdaadr(int s, int n);
extern SchObj subr_cdaar(int s, int n);
extern SchObj subr_cdadar(int s, int n);
extern SchObj subr_cdaddr(int s, int n);
extern SchObj subr_cdadr(int s, int n);
extern SchObj subr_cddaar(int s, int n);
extern SchObj subr_cddadr(int s, int n);
extern SchObj subr_cddar(int s, int n);
extern SchObj subr_cdddar(int s, int n);
extern SchObj subr_member(int s, int n);

extern int subr_string_is_equal_impl( SchObj x, SchObj y );
extern SchObj subr_string_is_equal(int s, int n);
extern SchObj subr_string_is_equal_ci(int s, int n);
extern SchObj subr_string_is_greater(int s, int n);
extern SchObj subr_string_is_greater_ci(int s, int n);
extern SchObj subr_string_is_less(int s, int n);
extern SchObj subr_string_is_less_ci(int s, int n);
extern SchObj subr_string_is_equal_or_greater(int s, int n);
extern SchObj subr_string_is_equal_or_greater_ci(int s, int n);
extern SchObj subr_string_is_equal_or_less(int s, int n);
extern SchObj subr_string_is_equal_or_less_ci(int s, int n);
extern SchObj subr_string_append( int s, int n );
extern SchObj subr_substring( int s, int n );
extern SchObj subr_map( int s, int n );
extern SchObj subr_list_ref( int s, int n );
extern SchObj subr_list_tail( int s, int n );
extern SchObj subr_reverse( int s, int n );
extern SchObj subr_list2vector( int s, int n );
extern SchObj subr_vector_fill( int s, int n );
extern SchObj subr_vector2list( int s, int n );
extern SchObj subr_string_copy( int s, int n );
extern SchObj subr_vector( int s, int n );

extern SchObj subr_is_input_port(int s, int n);
extern SchObj subr_is_output_port(int s, int n);
extern SchObj subr_open_input_file(int s, int n);
extern SchObj subr_open_output_file(int s, int n);
extern SchObj subr_close_input_port(int s, int n);
extern SchObj subr_close_output_port(int s, int n);
extern SchObj subr_standard_output_port(int s, int n);
extern SchObj subr_standard_input_port(int s, int n);
extern SchObj subr_current_output_port(int s, int n);
extern SchObj subr_current_input_port(int s, int n);
extern SchObj subr_is_eof_object(int s, int n);
extern SchObj subr_write(int s, int n);
extern SchObj subr_display(int s, int n);

extern SchObj subr_read_char(int s, int n);
extern SchObj subr_write_char(int s, int n);
extern SchObj subr_newline(int s, int n);
extern SchObj subr_read(int s, int n);

extern SchObj subr_foreach(int s, int n);
extern SchObj subr_load(int s, int n);

extern SchObj subr_gcd(int s, int n);
extern SchObj subr_lcm(int s, int n);
extern SchObj subr_is_even(int s, int n);
extern SchObj subr_is_odd(int s, int n);
extern SchObj subr_is_integer(int s, int n);
extern SchObj subr_modulo(int s, int n);
extern SchObj subr_remainder(int s, int n);



DisplayClosure* make_subr(SubrPnt fpnt,char* name);
DisplayClosure* make_subrs( SubrPnt* pnt, char** name );

SchObj vm ( int demand_insn_tbl, SchObj* x, DisplayClosure* c, int size, int sp, int fp );
int push_stk ( SchObj x, int s );


#endif  /* not INCLUDE_SUBR_H */
