/*--------------------------------------------------------------------
  numeric.c -  MameScheme ( Scheme Interpreter based on R5RS )
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
#include <ctype.h>

/* TODO 論理右シフトなのか算術論理シフトなのかをはっきりする */
#define DIGBITSIZE     (sizeof(int)*CHAR_BIT)
#define BIGNUM_HO(num) ((num)>>DIGBITSIZE)
#define BIGNUM_LO(num) ((num)&(UINT_MAX))

typedef unsigned long long BGDIGIT_DBL;
typedef unsigned int       BGDIGIT;

SchObj make_float(double d)
{
    SchFloat* ptr;
    ptr = SCH_MALLOC( sizeof(SchFloat) );
    ptr->hd.flags = T_FLOAT;
    ptr->d = d;
    return (SchObj)ptr;
}

SchBignum* newbignum( int len )
{
    SchBignum * n;
    n           = SCH_MALLOC( sizeof(SchBignum) );
    n->hd.flags = T_BIGNUM;
    n->len      = len;
    n->bytesize = len*sizeof(BGDIGIT);
    n->digits   = SCH_MALLOC( n->bytesize );
    n->sign     = 1;
    memset(n->digits,0,n->bytesize);

    return n;
}
SchBignum* int2bignum( int i )
{
    SchBignum* bgn = newbignum(1);

    bgn->sign = (i < 0) ? -1 : 1 ;
    ((BGDIGIT*)bgn->digits)[0] = (i < 0) ? i*-1 : i;

    return bgn;
}
SchBignum* uint2bignum( unsigned int i )
{
    SchBignum* bgn = newbignum(1);

    bgn->sign = 1;
    ((BGDIGIT*)bgn->digits)[0] = i;

    return bgn;
}
SchBignum* fix2bignum( SchObj n )
{
    return int2bignum(FIX2INT(n));
}
SchBignum* bignum_copy( SchBignum* src )
{
    int i;
    SchBignum * dst = newbignum(src->len);
    dst->sign = src->sign;

    i = 0;
    while ( i < src->len ) {
        ((BGDIGIT*)dst->digits)[i] = ((BGDIGIT*)src->digits)[i];
        i++;
    }

    return dst;
}
SchObj normalize_int( SchObj n )
{
    if ( FIXNUMP(n) ) {
        return n;
    } else if ( BIGNUMP(n) ) {

        SchBignum * bgn = SCH_BIGNUM_OBJ(n);
        int         i   = bgn->len;

        while ( i-- ) {
            if ( 0 != ((BGDIGIT*)bgn->digits)[i] ) {
                break;
            }
            bgn->len--;
        }

        bgn->bytesize = bgn->len*sizeof(BGDIGIT);

        if (bgn->len == 0 ) {
            return INT2FIX(0);
        } else if ( bgn->len == 1 ) {
            BGDIGIT digit = ((BGDIGIT*)bgn->digits)[0];

/*             int max = LONG_MAX; /\* 01111111111111111111111111111111 *\/ */
/*             int min = LONG_MIN; /\* 10000000000000000000000000000000 *\/ */
/*             max = LONG_MAX>>3;  /\* 00001111111111111111111111111111 *\/ */
/*             min = LONG_MIN>>3;  /\* 11110000000000000000000000000000 *\/ */

/*             int a = ((long)(digit*bgn->sign) <= (LONG_MAX>>3)) ? 0 : 1 ; */
/*             int b = ((LONG_MIN>>3) <= (long)(digit*bgn->sign)) ? 2 : 3 ; */

            if ( ((digit & (INT_MAX+1U)) == 0) && FIXABLE((long)digit*bgn->sign) ) {
                return INT2FIX(digit*bgn->sign);
            }
        }

        return bgn;
    }
}

int is_equal_num_int( SchObj l, SchObj r)
{
    l = normalize_int(l);
    r = normalize_int(r);

    if ( FIXNUMP(l) && FIXNUMP(r) ) {
        return ( l == r );
    } else if ( BIGNUMP(l) && BIGNUMP(r) ) {
        return ( cmp_abs_bignum(l,r) == 0 &&
                 (SCH_BIGNUM_OBJ(l)->sign == SCH_BIGNUM_OBJ(r)->sign) );
    } else {
        return 0;
    }
}
int is_equal_num_rational_float( SchRational* r, SchFloat* f )
{
    SchObj nmr = r->numerator;
    SchObj dnm = r->denominator;

    if ( FIXNUMP(nmr) && FIXNUMP(dnm) ){
        double d = 1.0 * FIX2INT(nmr) / FIX2INT(dnm);
        return ( d == f->d );
    } else {
        return 0;
    }
}
int is_equal_num( SchObj l, SchObj r)
{
    if ( (FIXNUMP(l) || BIGNUMP(l)) && (FIXNUMP(r) || BIGNUMP(r) ) ) {
        return is_equal_num_int(l,r);
    } else if ( FLOATP(l) && FLOATP(r) ) {
        return (SCH_FLOAT_OBJ(l)->d == SCH_FLOAT_OBJ(r)->d);
    } else if ( RATIONALP(l) && RATIONALP(r) ) {
        return ( (SCH_RATIONAL_OBJ(l)->numerator   == SCH_RATIONAL_OBJ(r)->numerator) &&
                 (SCH_RATIONAL_OBJ(l)->denominator == SCH_RATIONAL_OBJ(r)->denominator) );
    } else if ( RATIONALP(l) && FLOATP(r) ) {
        return is_equal_num_rational_float(SCH_RATIONAL_OBJ(l),SCH_FLOAT_OBJ(r));
    } else if ( FLOATP(l) && RATIONALP(r) ) {
        return is_equal_num_rational_float(SCH_RATIONAL_OBJ(r),SCH_FLOAT_OBJ(l));
    } else {
        return 0;
    }
}

/*
  abs(x) >  abs(y)  --->   1
  abs(x) == abs(y)  --->   0
  abs(x) <  abs(y)  --->  -1
 */
int cmp_abs_bignum( SchBignum* x, SchBignum* y )
{
    int max_len = ( x->len > y->len ) ? x->len : y->len ;
    int i       = max_len;
    BGDIGIT x_b, y_b;

    while ( i-- ) {

        if ( x->len > i ) {
            x_b = ((BGDIGIT*)x->digits)[i];
        } else {
            x_b = 0;
        }

        if ( y->len > i ) {
            y_b = ((BGDIGIT*)y->digits)[i];
        } else {
            y_b = 0;
        }

        if ( x_b > y_b ) {
            return 1;
        } else if ( x_b < y_b ) {
            return -1;
        }
    }

    return 0;
}


SchObj sum_abs_bignum( SchBignum* x_b, SchBignum* y_b )
{
    int len_x, len_y;

    len_x = x_b->len;
    len_y = y_b->len;

    SchBignum* bgn = newbignum( (len_x > len_y) ? len_x+1 : len_y+1 );

    BGDIGIT_DBL x_dg, y_dg, flow, digit;
    int i = 0;
    flow = 0;
    while ( bgn->len > i ) {

        if ( len_x > i ) {
            x_dg = ((BGDIGIT*)x_b->digits)[i];
        } else {
            x_dg = 0;
        }

        if ( len_y > i ) {
            y_dg = ((BGDIGIT*)y_b->digits)[i];
        } else {
            y_dg = 0;
        }

        digit = x_dg + y_dg + flow;

        ((BGDIGIT*)bgn->digits)[i] = BIGNUM_LO(digit);

        flow = BIGNUM_HO(digit);

        i++;
    }

    return bgn;
}


SchObj diff_abs_bignum( SchBignum* x_b, SchBignum* y_b )
{
    SchBignum * tmp;
    int len_x, len_y;

    int cmp = cmp_abs_bignum(x_b,y_b);

    if ( cmp == 0 ) {
        return INT2FIX(0);
    } else if ( cmp < 0 ) {
        tmp = x_b;
        x_b = y_b;
        y_b = tmp;
    }

    len_x = x_b->len;
    len_y = y_b->len;

    SchBignum* bgn = newbignum(len_x);

    long long x_dg, y_dg, flow, digit;
    int i = 0;
    flow = 0;
    while ( bgn->len > i ) {

        if ( len_x > i ) {
            x_dg = ((BGDIGIT*)x_b->digits)[i];
        } else {
            x_dg = 0;
        }

        if ( len_y > i ) {
            y_dg = ((BGDIGIT*)y_b->digits)[i];
        } else {
            y_dg = 0;
        }

        digit = x_dg - y_dg + flow;

        ((BGDIGIT*)bgn->digits)[i] = BIGNUM_LO(digit);

        if ( digit < 0 ) {
            flow = -1;
        } else {
            flow = BIGNUM_HO(digit);
        }

        i++;
    }

    return bgn;
}


SchObj add_int( SchObj x, SchObj y )
{
    SchBignum * x_b, * y_b;

    if ( FIXNUMP(x) ) {
        x_b = fix2bignum(x);
    } else {
        x_b = SCH_BIGNUM_OBJ(x);
    }

    if ( FIXNUMP(y) ) {
        y_b = fix2bignum(y);
    } else {
        y_b = SCH_BIGNUM_OBJ(y);
    }

    if ( x_b->sign == y_b->sign ) {

        SchBignum* s = sum_abs_bignum(x_b,y_b);
        s->sign = x_b->sign;
        return s;

    } else {

        SchBignum* d;
        int c = cmp_abs_bignum(x_b,y_b);

        if ( c == 0 ) {
            return INT2FIX(0);
        }

        d = diff_abs_bignum(x_b,y_b);
        d->sign = (c > 0) ? x_b->sign : y_b->sign ;
        return d;

    }

}

int add_rational( SchRational* r1, SchRational* r2 )
{
    SchObj r1n,r1d,r2n,r2d,nmr,dnm;

    if ( RATIONALP(r1) ) {
        r1n = r1->numerator;
        r1d = r1->denominator;
    } else {
        r1n = r1;
        r1d = INT2FIX(1);
    }
    if ( RATIONALP(r2) ) {
        r2n = r2->numerator;
        r2d = r2->denominator;
    } else {
        r2n = r2;
        r2d = INT2FIX(1);
    }

    nmr = add_int( mul_int(r1n, r2d),
                   mul_int(r1d, r2n) );

    dnm = mul_int(r1d,r2d);

    return SCH_RATIONAL(nmr,dnm);
}


SchObj sub_int( SchObj x, SchObj y )
{
    SchBignum * x_b, * y_b;

    if ( FIXNUMP(x) ) {
        x_b = fix2bignum(x);
    } else {
        x_b = SCH_BIGNUM_OBJ(x);
    }

    if ( FIXNUMP(y) ) {
        y_b = fix2bignum(y);
    } else {
        y_b = SCH_BIGNUM_OBJ(y);
    }

    if ( x_b->sign == y_b->sign ) {

        int c = cmp_abs_bignum(x_b,y_b);
        SchBignum* d;

        if ( c == 0 ) {
            return INT2FIX(0);
        }

        d = diff_abs_bignum(x_b,y_b);
        d->sign = ( c > 0 ) ? x_b->sign : y_b->sign*-1 ;

        return d;

    } else {

        SchBignum* s = sum_abs_bignum(x_b,y_b);
        s->sign = x_b->sign;

        return s;

    }
}


SchObj mul_bignum( SchBignum* x_b, SchBignum* y_b )
{
    int len_x, len_y;
    int i = 0;
    int j = 0;
    int k = 0;

    len_x = x_b->len;
    len_y = y_b->len;

    SchBignum* bgn = newbignum( (len_x > len_y) ? len_x*2 : len_y*2 );
    bgn->sign     = x_b->sign * y_b->sign;

    BGDIGIT_DBL x_dg, y_dg, dg_prd, ho;

    /*
        X3 X2 X1 X0    x_b ..... j
      * Y3 Y2 Y1 Y0    y_b ..... i
      -------------
    */

    while ( y_b->len > i ) {

        y_dg = ((BGDIGIT*)y_b->digits)[i];
        j    = 0;

        while ( x_b->len > j ) {

            x_dg   = ((BGDIGIT*)x_b->digits)[j];

            dg_prd = x_dg * y_dg + ((BGDIGIT*)bgn->digits)[i+j];


            ((BGDIGIT*)bgn->digits)[i+j] = BIGNUM_LO(dg_prd);

            k = i+j+1;
            while ( 0 != (ho = BIGNUM_HO(dg_prd)) ) {
                dg_prd = ho + ((BGDIGIT*)bgn->digits)[k];
                ((BGDIGIT*)bgn->digits)[k] = BIGNUM_LO(dg_prd);
                k++;
            }

            j++;
        }

        i++;
    }

    return bgn;
}

SchObj mul_int( SchObj x, SchObj y )
{
    SchBignum * x_b, * y_b;

    if ( FIXNUMP(x) ) {
        x_b = fix2bignum(x);
    } else {
        x_b = SCH_BIGNUM_OBJ(x);
    }

    if ( FIXNUMP(y) ) {
        y_b = fix2bignum(y);
    } else {
        y_b = SCH_BIGNUM_OBJ(y);
    }

    return normalize_int(mul_bignum(x_b,y_b));
}

SchObj quotient_int_impl( SchObj x, SchObj y, SchObj* rp )
{
#define BG_REWIND(__obj__,__dg__,__idx__)                               \
    do {                                                                \
        __idx__ = __obj__->len;                                         \
        while ( __idx__-- ) {                                           \
            if ( __dg__ = ((BGDIGIT*)__obj__->digits)[__idx__] ) break; \
        }                                                               \
    } while(0)


    SchBignum * x_b, * y_b;
    int xi, yi, c;
    BGDIGIT_DBL xd, yd;

    x_b = FIXNUMP(x) ? fix2bignum(x) : SCH_BIGNUM_OBJ(x);
    y_b = FIXNUMP(y) ? fix2bignum(y) : SCH_BIGNUM_OBJ(y);

    c = cmp_abs_bignum(x_b , y_b);

    if ( c < 0 ) {
        *rp = normalize_int(x_b);
        return INT2FIX(0);
    } else if ( c == 0 ) {
        *rp = INT2FIX(0);
        return INT2FIX(x_b->sign*y_b->sign);
    }

    BG_REWIND(x_b,xd,xi);
    BG_REWIND(y_b,yd,yi);

    int bsc = 0;
    BGDIGIT dg = yd;
    while ( (dg & (1U<<(DIGBITSIZE-1))) == 0 ) {
        dg <<= 1;
        bsc++;
    }

    SchBignum* u_b = newbignum(x_b->len+1);
    SchBignum* v_b = bignum_copy(y_b);
    u_b->sign = x_b->sign;

    if ( bsc ) {
        BGDIGIT* xds = (BGDIGIT*)x_b->digits;
        BGDIGIT* yds = (BGDIGIT*)y_b->digits;
        BGDIGIT* uds = (BGDIGIT*)u_b->digits;
        BGDIGIT* vds = (BGDIGIT*)v_b->digits;
        BGDIGIT_DBL td = 0;
        int j = 0;

        while ( j <= yi ) {
            td += (BGDIGIT_DBL)yds[j] << bsc;
            vds[j] = BIGNUM_LO(td);
            td = BIGNUM_HO(td);
            j++;
        }

        td = 0;
        j  = 0;
        while ( j <= xi ) {
            td += (BGDIGIT_DBL)xds[j] << bsc;
            uds[j] = BIGNUM_LO(td);
            td = BIGNUM_HO(td);
            j++;
        }
        uds[j] = td;
    } else {
        u_b = x_b;
    }

    BG_REWIND(v_b,yd,yi);

    SchBignum* q, *q0, *r_b, *p;
    SchObj r;
    BGDIGIT_DBL qd;

    q       = newbignum( x_b->len - (y_b->len - 1) );
    q->sign = (x_b->sign) * (y_b->sign);

    while ( 1 ) {

        BG_REWIND(u_b,xd,xi);

        if ( yd > xd ) {
            xd = (xd << DIGBITSIZE) + ((BGDIGIT*)u_b->digits)[--xi];
        }

        qd = xd / yd;

        q0       = newbignum(xi+1);
        q0->sign = (u_b->sign) * (v_b->sign);

        while ( 1 ) {

            ((BGDIGIT*)q->digits)[xi-yi]  = qd;
            ((BGDIGIT*)q0->digits)[xi-yi] = qd;

            p  = mul_bignum(v_b,q0);

            if ( cmp_abs_bignum(u_b,p) < 0 ) {
                qd--;
            } else {
                break;
            }
        }

        r   = sub_int( u_b, p );
        r_b = FIXNUMP(r) ? fix2bignum(r) : SCH_BIGNUM_OBJ(r);

        if ( 0 > cmp_abs_bignum(r_b,v_b) ) {
            if ( bsc ) {
                BGDIGIT_DBL rd;
                BGDIGIT   * rds = (BGDIGIT*)r_b->digits;
                BGDIGIT_DBL crr = 0;
                int         ri = r_b->len;
                while ( ri-- ) {
                    rd = crr + (BGDIGIT_DBL)rds[ri];
                    if ( rd == 0 ) continue;
                    crr = (rd & ((1U<<bsc)-1)) << DIGBITSIZE;
                    rds[ri] = rd>>bsc;
                }
            }
            *rp = normalize_int(r_b);
            return normalize_int(q);
        } else {
            u_b = r_b;
        }
    }
}

SchObj quotient_int( SchObj x, SchObj y )
{
    SchObj r;
    SchObj q = quotient_int_impl(x,y,&r);

    return q;
}

SchObj remainder_int( SchObj x, SchObj y )
{
    SchObj r;
    quotient_int_impl(x,y,&r);

    return r;
}

int gcd_fixnum(int x, int y)
{
    while (1) {
        x = x % y;
        if (x == 0) {
            return y;
        }
        y = y % x;
        if (y == 0) {
            return x;
        }
    }
}
SchObj gcd_int( SchObj x, SchObj y )
{
    if ( FIXNUMP(x) && FIXNUMP(y) ) {
        return INT2FIX(gcd_fixnum(FIX2INT(x),FIX2INT(y)));
    }

    while ( 1 ) {
        x = remainder_int(x,y);
        if ( x == INT2FIX(0) ) {
            return y;
        }
        y = remainder_int(y,x);
        if ( y == INT2FIX(0) ) {
            return x;
        }
    }
}

int lcm_fixnum(int x, int y)
{
    return x * y / gcd_fixnum(x,y);
}
SchObj lcm_int(SchObj x, SchObj y)
{
    return quotient_int(mul_int(x,y),gcd_int(x,y));
}

SchObj rational_fixnum(int n, int d)
{
    SchRational* ptr;
    int factor;
    int n1,d1;

    factor = gcd_fixnum(n,d);

    if ( factor == d ) {
        return (SchObj)INT2FIX(n/d);
    }

    n1 = n/factor;
    d1 = d/factor;

    if ( d1 < 0 ) {
        d1 *= -1;
        n1 *= -1;
    }

    ptr = SCH_MALLOC( sizeof(SchRational) );
    ptr->hd.flags    = T_RATIONAL;
    ptr->numerator   = INT2FIX(n1);
    ptr->denominator = INT2FIX(d1);

    return (SchObj)ptr;
}

int is_negative(SchObj n) {
    if ( FIXNUMP(n) ) {
        return ( FIX2INT(n) < 0 );
    } else if ( BIGNUMP(n) ) {
        return ( SCH_BIGNUM_OBJ(n)->sign < 0 );
    } else if ( RATIONALP(n) ) {
        return ( (is_negative(SCH_RATIONAL_OBJ(n)->denominator)?1:0) ^ (is_negative(SCH_RATIONAL_OBJ(n)->numerator)?1:0 ));
    } else if ( FLOATP(n) ) {
        return ( SCH_FLOAT_OBJ(n)->d < 0 );
    } else {
        return 0;
    }
}

SchObj invert_sign(SchObj n) {
    SchObj n0 = SCH_NIL;
    if ( FIXNUMP(n) ) {
        return INT2FIX(FIX2INT(n)*-1);
    } else if ( BIGNUMP(n) ) {
        n0 = bignum_copy(n);
        SCH_BIGNUM_OBJ(n0)->sign *= -1;
    } else if ( FLOATP(n) ) {
        n0 = SCH_FLOAT( SCH_FLOAT_OBJ(n)->d * -1 );
    } else if ( RATIONALP(n) ) {
        n0 = SCH_RATIONAL( invert_sign(SCH_RATIONAL_OBJ(n)->numerator),
                           SCH_RATIONAL_OBJ(n)->denominator );
    }
    return n0;
}

SchObj rational(SchObj n, SchObj d)
{
    SchRational* ptr;
    SchObj factor,n1,d1;

    if (FIXNUMP(n) && FIXNUMP(d)) {
        return rational_fixnum(FIX2INT(n),FIX2INT(d));
    }

    factor = gcd_int(n,d);

    if ( is_equal_num(factor,d) ) {
        return (SchObj)quotient_int(n,d);
    }
    n1 = quotient_int(n,factor);
    d1 = quotient_int(d,factor);
    if ( is_negative(d1) ) {
        n1 = mul_int(n1,(INT2FIX(-1)));
        d1 = mul_int(d1,(INT2FIX(-1)));
    }
    ptr = SCH_MALLOC( sizeof(SchRational) );
    ptr->hd.flags    = T_RATIONAL;
    ptr->numerator   = n1;
    ptr->denominator = d1;

    return (SchObj)ptr;
}

SchObj str2bignum(char* str, int base)
{
#define conv_digit(c) \
    (!isascii(c) ? -1 : \
     isdigit(c) ? ((c) - '0') : \
     islower(c) ? ((c) - 'a' + 10) : \
     isupper(c) ? ((c) - 'A' + 10) : \
     -1)

    SchBignum * ptr;
    BGDIGIT * digits;

    int scale, bitsize, len, i;
    int sign = 1;
    int blen = 1;
    BGDIGIT_DBL num  = 0;

    if ( str[0] == '-' ) {
        sign = -1;
        str++;
    } else if (str[0] == '+') {
        sign = 1;
        str++;
    }

    while ( str[0] == '0' ) {
        str++;
    }

    switch ( base ) {
    case 2:
        scale = 1; break;
    case 3:
    case 4:
        scale = 2; break;
    case 5:
    case 6:
    case 7:
    case 8:
        scale = 3; break;
    case  9:
    case 10:
    case 16:
        scale = 4; break;
    default:
        scale = 6; break;
    }

    bitsize = strlen(str) * scale;
    len     = (bitsize/(sizeof(int)*CHAR_BIT)); /*    bitsize / 4*8   */

    if ( bitsize % (sizeof(int)*CHAR_BIT) ) {
        len += 1;
    }

    ptr       = newbignum(len);
    ptr->sign = sign;
    digits    = (BGDIGIT*)ptr->digits;

    while ( *str ) {

        num  = conv_digit((int)*str);
        i    = 0;

        while ( i<blen ) {
            num += ((BGDIGIT_DBL)digits[i])*((BGDIGIT)base);
            digits[i] = BIGNUM_LO(num);
            num = BIGNUM_HO(num);
            i++;
            if ( num > 0 && i == blen ) {
                blen++;
            }
        }
        str++;
    }

    return (SchObj)ptr;
}

char* bignum2str(SchBignum* obj, int base)
{
#define DIVISOR_SZ 4
#define BIGNUM_UP(num) ((num)<<(sizeof(int)*CHAR_BIT))

    static char *num_chars = "0123456789abcdefghijklmnopqrstuvwxyz";

    int hbase = base*base;
    char *str, *rstr;
    int sign = obj->sign;
    int scale, bsize, len;
    BGDIGIT     dj;
    BGDIGIT   * digits;
    SchBignum * work = bignum_copy(obj);

    digits = (BGDIGIT*)work->digits;

    switch ( base ) {
    case 2:
    case 3:
        scale = 1; break;
    case 4:
    case 5:
    case 6:
    case 7:
        scale = 2; break;
    case 8:
    case 9:
    case 10:
    case 11:
    case 12:
    case 13:
    case 14:
    case 15:
        scale = 3; break;
    default:
        scale = 4; break;
    }

    bsize = obj->bytesize * CHAR_BIT / scale + 1;

    if ( sign < 0 ) bsize += 1;

    str = SCH_MALLOC(bsize);
    memset( str, '0', bsize );

    rstr = str + bsize - 1;
    rstr[0] = '\0';

#if DIVISOR_SZ > 2
    hbase *= hbase;
#endif

    do {
        BGDIGIT_DBL wd = 0;
        int         i  = obj->len;
        char        tbuf[DIVISOR_SZ+1];
        dj = 0;

        while ( i-- ) {
            wd = BIGNUM_UP(wd) + digits[i];
            digits[i] = wd/hbase;
            wd %= hbase;
            dj |= digits[i];
        }

        i = DIVISOR_SZ;
        tbuf[i] = '\0';
        while ( i-- ) {
            tbuf[i] = num_chars[wd%base];
            wd /= base;
        }

        rstr -= strlen( tbuf );
        strncpy( rstr, (tbuf), strlen(tbuf) );

    } while ( dj );

    while ( rstr[0] == '0' ) {
        rstr++;
    }

    len = strlen(rstr);
    if ( sign > 0 ) {
        memmove( str, rstr, len );
        str[len] = '\0';
    } else {
        memmove( str+1, rstr, len );
        str[0]     = '-';
        str[len+1] = '\0';
    }

    return str;
}
