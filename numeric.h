/*--------------------------------------------------------------------
  numeric.h -  MameScheme ( Scheme Interpreter based on R5RS )
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

#ifndef INCLUDE_NUMERIC_H
#define INCLUDE_NUMERIC_H

SchObj normalize_int( SchObj n );
SchObj remainder_int( SchObj x, SchObj y );
int is_equal_num( SchObj l, SchObj r);
int cmp_num( SchObj x, SchObj y );
SchBignum* int2bignum( int i );
int add_rational( SchObj r1, SchObj r2 );
SchObj mul_rational( SchObj x, SchObj y );
SchObj invert_sign(SchObj n);
int sub_rational( SchObj r1, SchObj r2 );
SchObj quotient_int( SchObj x, SchObj y );
SchObj gcd_int( SchObj x, SchObj y );
SchObj lcm_int(SchObj x, SchObj y);
int cmp_abs_bignum( SchBignum* x, SchBignum* y );
int subr_is_eqv_impl( SchObj arg1, SchObj arg2 );

#endif
