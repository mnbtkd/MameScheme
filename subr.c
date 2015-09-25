/*--------------------------------------------------------------------
  subr.c -  MameScheme ( Scheme Interpreter based on R5RS )
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
#include "subr.h"
#include <ctype.h>




/*------------------------------------------------------------------------------------
    SUBR
  ------------------------------------------------------------------------------------ */

DisplayClosure* make_subr(SubrPnt fpnt,char* name)
{
    DisplayClosure* subr;
    subr = SCH_MALLOC(sizeof(DisplayClosure));
    subr->hd.flags = T_SUBR;
    subr->u.subr    = fpnt;
    subr->vars_size = 0;
    subr->name = name;
    return subr;
}

static DisplayClosure* make_subrs_impl ( SubrPnt* pnt, char** name, int size )
{
    DisplayClosure* ret;
    SchObj subr;
    int i;

    ret = SCH_MALLOC(sizeof(DisplayClosure));
    ret->vars = SCH_MALLOC(sizeof(SchObj)*size);

    for ( i=0; pnt[i] ; ++i ) {
        subr = (SchObj)make_subr(pnt[i],name[i]);
        ret->vars[i] = subr;
        ret->hd.flags = T_SUBR;
    }
    ret->vars_size = i;

    return ret;
}




/* ------ equivalence --------------------------------------------------- */
int subr_is_eq_impl( SchObj x, SchObj y )
{
    return (x == y);
}
SchObj subr_is_eq( int s, int n )
{
    SchObj x, y;
    x = POP_STACK(s);
    y = POP_STACK(s);
    return (subr_is_eq_impl(x,y) ? SCH_TRUE : SCH_FALSE);
}

int subr_is_eqv_impl( SchObj arg1, SchObj arg2 )
{
    int type1, type2;
    type1 = SCH_TYPE(arg1);
    type2 = SCH_TYPE(arg2);

    if ( type1 != type2 ) return 0;

    switch ( type1 ) {
    case T_FIXNUM:
    case T_BIGNUM:
    case T_RATIONAL:
    case T_FLOAT:
        /* TODO check exact or not. */
        return is_equal_num(arg1,arg2);
    case T_SYMBOL:       return (strcmp(SCH_ID2NAME(arg1), SCH_ID2NAME(arg2))==0);
    case T_CHAR:         return (CHAR_EQUALP(arg1,arg2) == SCH_TRUE);
    case T_STRING:                /* FALLTHROU */
    case T_PAIR:                  /* FALLTHROU */
    case T_VECTOR:                /* FALLTHROU */
    case T_MISC_UNDEFINE:         /* FALLTHROU */
    case T_SUBR:                  /* FALLTHROU */
    case T_LAMBDA:       return (arg1==arg2);
    case T_MISC_NIL:     return 1;
    case T_MISC_FALSE:   return 1;
    case T_MISC_TRUE:    return 1;
    case T_ENV:                   /* FALLTHROU */
    case T_MISC_EOF:              /* FALLTHROU */
    case T_MISC_UNBOUND:          /* FALLTHROU */
    case T_MISC_DOT:              /* FALLTHROU */
    case T_MISC_KOKKA:            /* FALLTHROU */
    default:
        return 0;
    }
}

SchObj subr_is_eqv ( int s, int n )
{
    SchObj x;
    SchObj y;
    x = POP_STACK(s);
    y = POP_STACK(s);
    return ( subr_is_eqv_impl(x,y) ? SCH_TRUE : SCH_FALSE );
}

int subr_is_equal_impl ( SchObj x, SchObj y )
{
    int type1, type2;

    type1 = SCH_TYPE(x);
    type2 = SCH_TYPE(y);

    if ( type1 != type2 ) {
        return 0;
    }

    switch ( type1 ) {
    case T_STRING: return subr_string_is_equal_impl(x,y);/* (strcmp(SCH_STRING_OBJ(x)->buf, SCH_STRING_OBJ(y)->buf) == 0); */
    case T_PAIR:
    {
        SchObj SCH_MANGLE(tmp);
        SchObj ex,ey;
        FOR_EACH_2LISTS(ex,x,ey,y) {
            if ( ! subr_is_equal_impl(ex,ey) ) {
                return 0;
            }
        }
        return 1;
    }
    case T_VECTOR:
    {
        SchVector* v1, * v2;
        int i,size;
        v1 = SCH_VECTOR_OBJ(x);
        v2 = SCH_VECTOR_OBJ(y);
        size = v1->size;

        for (i=0;i<size;++i) {
            if ( PAIRP(v1->vec[i])||VECTORP(v1->vec[i])||STRINGP(v1->vec[i]) ) {
                if ( ! subr_is_equal_impl(v1->vec[i],v2->vec[i]) ) {
                    return 0;
                }
            } else {
                if ( ! subr_is_eqv_impl(v1->vec[i],v2->vec[i]) ) {
                    return 0;
                }
            }
        }
        return 1;
    }
    case T_FIXNUM:                /* FALLTHROU */
    case T_BIGNUM:                /* FALLTHROU */
    case T_SYMBOL:                /* FALLTHROU */
    case T_CHAR:                  /* FALLTHROU */
    case T_SUBR:                  /* FALLTHROU */
    case T_LAMBDA:                /* FALLTHROU */
    case T_MISC_NIL:              /* FALLTHROU */
    case T_MISC_FALSE:            /* FALLTHROU */
    case T_MISC_TRUE:             /* FALLTHROU */
    case T_ENV:                   /* FALLTHROU */
    case T_MISC_EOF:              /* FALLTHROU */
    case T_MISC_UNBOUND:          /* FALLTHROU */
    case T_MISC_UNDEFINE:         /* FALLTHROU */
    case T_MISC_DOT:              /* FALLTHROU */
    case T_MISC_KOKKA:            /* FALLTHROU */
    default:
/*         return ( subr_is_eqv_impl(x,y) ? SCH_TRUE : SCH_FALSE ); */
        return subr_is_eqv_impl(x,y);
    }

}

SchObj subr_is_equal ( int s, int n )
{
    SchObj x;
    SchObj y;
    x = POP_STACK(s);
    y = POP_STACK(s);
    return ( subr_is_equal_impl(x,y) ? SCH_TRUE : SCH_FALSE );
}

/* ------ arithmetic ---------------------------------------------------- */
SchObj subr_is_equal_num(int s, int n)
{
    SchObj f;
    SchObj x;
    f = POP_STACK(s); n--;
    LOOP_STACK(s,n,x){
        if ( is_equal_num(f,x) ) {
        } else {
            return SCH_FALSE;
        }
    }
    return SCH_TRUE;
/*     SchObj f; */
/*     SchObj x; */
/*     f = POP_STACK(s); n--; */
/*     LOOP_STACK(s,n,x){ */
/*         if ( f == x) { */
/*         } else { */
/*             return SCH_FALSE; */
/*         } */
/*     } */
/*     return SCH_TRUE; */
}

SchObj subr_is_greater(int s, int n)
{
    SchObj r;
    SchObj l;
    l = POP_STACK(s); n--;
    LOOP_STACK(s,n,r) {
        if ( FIXNUMP(l) && FIXNUMP(r) ) {
            if ( !( FIX2INT(l) < FIX2INT(r) ) ) {
                return SCH_FALSE;
            }
        } else {
            if ( cmp_num(l,r) != -1 ) {
                return SCH_FALSE;
            }
        }
        l = r;
    }
    return SCH_TRUE;
}

SchObj subr_is_equal_to_or_greater(int s, int n)
{
    SchObj r;
    SchObj l;
    l = POP_STACK(s); n--;
    LOOP_STACK(s,n,r) {
        if ( FIXNUMP(l) && FIXNUMP(r) ) {
            if ( FIX2INT(l) > FIX2INT(r) ) {
                return SCH_FALSE;
            }
        } else {
            if ( cmp_num(l,r) > 0 ) {
                return SCH_FALSE;
            }
        }
        l = r;
    }
    return SCH_TRUE;
}

SchObj subr_is_less(int s, int n)
{
    SchObj r;
    SchObj l;
    l = POP_STACK(s); n--;
    LOOP_STACK(s,n,r){
        if ( FIXNUMP(l) && FIXNUMP(r) ) {
            if ( !( FIX2INT(l) > FIX2INT(r) ) ) {
                return SCH_FALSE;
            }
        } else {
            if ( cmp_num(l,r) != 1 ) {
                return SCH_FALSE;
            }
        }
        l = r;
    }
    return SCH_TRUE;
}

SchObj subr_is_equal_to_or_less(int s, int n)
{
    SchObj r;
    SchObj l;
    l = POP_STACK(s); n--;
    LOOP_STACK(s,n,r){
        if ( FIXNUMP(l) && FIXNUMP(r) ) {
            if ( FIX2INT(l) < FIX2INT(r) ) {
                return SCH_FALSE;
            }
        } else {
            if ( cmp_num(l,r) < 0 ) {
                return SCH_FALSE;
            }
        }
        l = r;
    }
    return SCH_TRUE;
}

SchObj subr_is_zero ( int s, int n )
{
    SchObj x = POP_STACK(s);
    return ( FIX2INT(x) == 0 ) ? SCH_TRUE : SCH_FALSE ;
}

SchObj subr_add(int s, int n)
{
    SchObj x;
    SchObj ret = INT2FIX(0);

    int    iret = 0;
    double dret = 0;
    SchObj bret = INT2FIX(0);
    SchObj rret = INT2FIX(0);

    int    d_count = 0;
    int    b_count = 0;
    int    r_count = 0;

    LOOP_STACK(s,n,x){
        if ( FIXNUMP(x) ) {
            iret += FIX2INT(x);
            if ( ! FIXABLE(iret) ) {
                bret = add_int( bret, int2bignum(iret) );
                b_count++;
                iret = 0;
            }
        } else if ( FLOATP(x) ) {
            dret += SCH_FLOAT_OBJ(x)->d;
            d_count++;
        } else if ( BIGNUMP(x) ) {
            bret = add_int(bret,x);
            b_count++;
        } else if ( RATIONALP(x) ) {
            rret = add_rational(rret,x);
            r_count++;
        }
    }

    /*
      iret
      iret bret
      iret      rret
      iret bret rret
      iret           dret
      iret bret      dret
      iret      rret dret
      iret bret rret dret
     */

    /* i */
    if ( (b_count|d_count|r_count) == 0 ) {
        return INT2FIX(iret);
    }

    /* b */
    if ( (d_count|r_count|iret) == 0 ) {
        return bret;
    }

    /* r */
    if ( (iret|b_count|d_count) == 0 ) {
        return rret;
    }

    /* i d */
    if ( (b_count|r_count) == 0 ) {
        return SCH_FLOAT(iret+dret);
    }

    /* i b */
    if ( (d_count|r_count) == 0 ) {
        return add_int(INT2FIX(iret),bret);
    }

    /* i r */
    if ( (d_count|b_count) == 0 ) {
        return add_rational(INT2FIX(iret),rret);
    }

    /* b r */
    if ( (iret|d_count) == 0 ) {
        return add_rational(bret,rret);
    }

    /* d b */
    if ( (iret|r_count) == 0 ) {
        return SCH_UNDEFINE;
    }

    /* d r */
    if ( (iret|b_count) == 0 ) {
        return SCH_UNDEFINE;
    }

    /* i b   r */
    if ( d_count == 0 ) {
        SchObj tmp = add_int(INT2FIX(iret),bret);
        return add_rational(tmp,rret);
    }

    /*   b d r */
    if ( iret == 0 ) {
        return SCH_UNDEFINE;
    }
    /* i   d r */
    if ( b_count == 0 ) {
        return SCH_UNDEFINE;
    }
    /* i b d   */
    if ( r_count == 0 ) {
        return SCH_UNDEFINE;
    }

    /* i b d r */
    return SCH_UNDEFINE;

}

size_t idx_mostleft_bit_abs(int i) {

    size_t idx = 0;
    int    i0  = i;

    if ( i0 < 0 ) i0 *= -1;

    while ( i0 = i0>>1 ) {
        idx++;
    }
    return idx;
}

SchObj subr_mul(int s, int n)
{
    SchObj x;
    SchObj ret     = INT2FIX(1);
    int    iret    = 1;
    int    i0      = 1;
    double dret    = 1.0;
    SchObj bret    = INT2FIX(1);
    SchObj rret    = INT2FIX(1);

    int    d_count = 0;
    int    b_count = 0;
    int    r_count = 0;

    LOOP_STACK(s,n,x){
        if (FIXNUMP(x)) {
            i0 = FIX2INT(x);
            /* overflow checking*/
            if ( (idx_mostleft_bit_abs(iret)+1 + idx_mostleft_bit_abs(i0)+1) > (idx_mostleft_bit_abs(LONG_MAX>>3)+1) ) {
                /* overflowed */
                /* printf("overflowed iret:%d  i0:%d    ireb.mlb:%d i0.mlb:%d  LONG_MAX.mlb%d\n", */
                /*        iret, */
                /*        i0, */
                /*        idx_mostleft_bit_abs(iret)+1, */
                /*        idx_mostleft_bit_abs(i0)+1, */
                /*        (idx_mostleft_bit_abs(LONG_MAX>>3)+1) */
                /*        ); */
                bret = mul_int( mul_int( INT2FIX(iret),
                                         x ),
                                bret );
                iret = 1;
                b_count++;
            } else {
                /* not overflowed */
                iret *= i0;
            }

        } else if ( FLOATP(x) ) {
            dret *= SCH_FLOAT_OBJ(x)->d;
            d_count++;
        } else if ( BIGNUMP(x) ) {
            bret = mul_int(bret,x);
            b_count++;
        } else if ( RATIONALP(x) ) {
            rret = mul_rational(rret,x);
            r_count++;
        }
    }

    if ( (b_count|d_count|r_count) == 0 ) {
        return INT2FIX(iret);
    }

    if ( (b_count|d_count) == 0) {
        if ( iret == 1 ) return rret;
        return mul_rational(INT2FIX(iret),rret);
    }

    if ( b_count != 0 ) {
        if ( iret ) {
            ret = mul_int(INT2FIX(iret),bret);
        } else {
            ret = bret;
        }
    }

    if ( d_count == 0 ) {
        return ret;
    } else {
        if ( b_count != 0 ) {
            return SCH_NIL;     /* TODO implement */
        } else {
            return SCH_FLOAT( iret*dret );
        }
    }
}

SchObj subr_sub(int s, int n)
{
    SchObj x;
    SchObj sum;
    int    s0 = s;
    int    n0 = n;

    x = POP_STACK(s0); n0--;

    if ( n0 < 1 ) {
        return invert_sign(x);
    }

    sum = subr_add(s0,n0);

    if ( FIXNUMP(x) && FIXNUMP(sum) ) {
        int dff = FIX2INT(x) - FIX2INT(sum);
        if ( FIXABLE(dff) ) {
            return INT2FIX(dff);
        } else {
            return int2bignum(dff);
        }
    }

    if ( ( FIXNUMP(x) && BIGNUMP(sum) ) ||
         ( BIGNUMP(x) && FIXNUMP(sum) ) ||
         ( BIGNUMP(x) && BIGNUMP(sum) ) ) {

        return sub_int( x, sum );
    }

    if ( ( RATIONALP(x) && RATIONALP(sum) ) ||
         ( RATIONALP(x) && ( FIXNUMP(sum) || BIGNUMP(sum) ) ) ||
         ( FIXNUMP(x) || BIGNUMP(x) ) && RATIONALP(sum) ) {
        return sub_rational( x, sum );
    }

    if ( FIXNUMP(x) && FLOATP(sum) ) {
        return SCH_FLOAT( FIX2INT(x) - SCH_FLOAT_OBJ(sum)->d );
    }

    if ( FLOATP(x) && FIXNUMP(sum) ) {
        return SCH_FLOAT( SCH_FLOAT_OBJ(x)->d - FIX2INT(sum) );
    }

    return SCH_NIL;
}

SchObj subr_div(int s, int n)
{
    SchObj numer = INT2FIX(1);
    SchObj denom = INT2FIX(1);
    SchObj x;

    x = POP_STACK(s); n--;

    if ( n < 1 ) {
        if ( RATIONALP(x) ) {
            numer = SCH_RATIONAL_OBJ(x)->denominator;
            denom = SCH_RATIONAL_OBJ(x)->numerator;
        } else {
            denom = x;
        }
    } else {
        if ( RATIONALP(x) ) {
            numer = SCH_RATIONAL_OBJ(x)->numerator;
            denom = SCH_RATIONAL_OBJ(x)->denominator;
        } else {
            numer = x;
        }
    }

    LOOP_STACK(s,n,x){
        if ( RATIONALP(x) ) {
            numer = mul_int(numer,SCH_RATIONAL_OBJ(x)->denominator);
            x = SCH_RATIONAL_OBJ(x)->numerator;
        }

        denom = mul_int(denom,x);
    }
    return SCH_RATIONAL(numer,denom);

}

SchObj subr_quotient(int s, int n)
{
    SchObj x,y;
    x = POP_STACK(s);
    y = POP_STACK(s);
    return quotient_int(x,y);
}

SchObj number2string_impl(int i, int r)
{
    char* str;
    str = SCH_MALLOC(sizeof(char)*64); /* TODO hard coding */

    switch(r){
    case 10: sprintf(str,"%d",i); break;
    case 8:  sprintf(str,"%o",i); break;
    case 16: sprintf(str,"%x",i); break;
    default: sprintf(str,"%d",i); break;
    }

    return SCH_STRING(str);
}

SchObj subr_number2string_with_rad ( int s, int n )
{
    int i;
    int r;
    SchObj x = POP_STACK(s);
    SchObj y = POP_STACK(s);
    r = FIX2INT(y);
    i = FIX2INT(x);

    return number2string_impl(i,r);
}

SchObj subr_number2string ( int s, int n )
{
    if ( n == 2 ) {
        return subr_number2string_with_rad(s,n);
    } else {
        int i  = FIX2INT(POP_STACK(s));
        return number2string_impl(i,10);
    }
}

SchObj subr_is_number ( int s, int n )
{
    SchObj x = POP_STACK(s);
    return ((FIXNUMP(x)||FLOATP(x)||RATIONALP(x)||BIGNUMP(x)) ? SCH_TRUE : SCH_FALSE);
}

SchObj subr_gcd(int s, int n)
{
    SchObj x,x0;

    if ( n == 0 ) {
        return INT2FIX(0);
    } else if ( n == 1 ) {
        return POP_STACK(s);
    } else {

        x0 = POP_STACK(s); --n;

        LOOP_STACK(s,n,x){
            x0 = gcd_int(x0,x);
        }

        return x0;
    }

}

SchObj subr_lcm(int s, int n)
{
    SchObj x,x0;
    int    m;

    if ( n == 0 ) {
        return INT2FIX(1);
    } else if ( n == 1 ) {
        return POP_STACK(s);
    } else {

        x0 = POP_STACK(s); --n;

        LOOP_STACK(s,n,x){
            x0 = lcm_int(x0,x);
        }

        return x0;
    }
}

SchObj subr_is_even(int s, int n)
{
    SchObj x;
    int i;

    x = POP_STACK(s);
    i = FIX2INT(x);

    return ((i%2) == 0) ? SCH_TRUE : SCH_FALSE ;
}
SchObj subr_is_odd(int s, int n)
{
    SchObj x;
    int i;

    x = POP_STACK(s);
    i = FIX2INT(x);

    return ((i%2) == 0) ? SCH_FALSE : SCH_TRUE ;
}

SchObj subr_is_integer(int s, int n)
{
    SchObj x;

    x = POP_STACK(s);
    return ((FIXNUMP(x))||BIGNUMP(x)) ? SCH_TRUE : SCH_FALSE ;
}

SchObj subr_modulo(int s, int n)
{
#define INT_SIGN(_x_)    ((FIXNUMP(_x_)) ? ((FIX2INT(_x_)<0) ? -1 : 1) : \
                                           SCH_BIGNUM_OBJ(_x_)->sign)
    SchObj x,y,r;
    int x_sign,y_sign;

    x  = POP_STACK(s);
    y  = POP_STACK(s);

    r  = remainder_int(x,y);

    x_sign = INT_SIGN(x);
    y_sign = INT_SIGN(y);

    if ( x_sign != y_sign ) {
        r = add_int(r,y);
    }

    return normalize_int(r);
}
SchObj subr_remainder(int s, int n)
{
    SchObj x,y;

    x = POP_STACK(s);
    y = POP_STACK(s);

    return remainder_int(x,y);
}
/* ------ boolean ----------------------------------------------------------- */
SchObj subr_not ( int s, int n )
{
    SchObj x = POP_STACK(s);
    return (SCH_FALSE == x) ? SCH_TRUE : SCH_FALSE ;
}

SchObj subr_is_boolean ( int s, int n )
{
    SchObj x = POP_STACK(s);
    return ( BOOLEANP(x) ? SCH_TRUE : SCH_FALSE );
}

/* ------ pair -------------------------------------------------------------- */
SchObj subr_is_pair ( int s, int n )
{
    SchObj x = POP_STACK(s);
    return (PAIRP(x) ? SCH_TRUE : SCH_FALSE);
}

SchObj subr_cons(int s, int n)
{
    SchObj kar;
    SchObj kdr;
    kar = POP_STACK(s);
    kdr = POP_STACK(s);
    return SCH_CONS(kar,kdr);
}

SchObj subr_car(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CAR(x);
}

SchObj subr_cdr(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CDR(x);
}

SchObj subr_set_car(int s, int n)
{
    SchObj pair;
    SchObj obj;
    pair = POP_STACK(s);
    obj  = POP_STACK(s);
    SCH_SET_CAR(pair,obj);
    return SCH_UNDEFINE;
}

SchObj subr_set_cdr(int s, int n)
{
    SchObj pair;
    SchObj obj;
    pair = POP_STACK(s);
    obj  = POP_STACK(s);
    SCH_SET_CDR(pair,obj);
    return SCH_UNDEFINE;
}

SchObj subr_is_null ( int s, int n )
{
    return (POP_STACK(s) == SCH_NIL) ? SCH_TRUE : SCH_FALSE;
}

SchObj subr_list(int s, int n)
{
    SchObj lst;
    SchObj x;
    x   = INDEX_ST(s,--n);
    lst = SCH_CONS(x,SCH_NIL);

    while ( n-- ) {
        x = INDEX_ST(s,n);
        lst = SCH_CONS(x,lst);
    }

    return lst;
}

SchObj subr_vector(int s, int n)
{
    SchObj vec = SCH_VECTOR(n);
    SchObj x;
    while ( n-- ) {
        x = INDEX_ST(s,n);
        SCH_VECTOR_REF(vec,n) = x;
    }
    return vec;
}

SchObj subr_length ( int s, int n )
{
    SchObj x      = POP_STACK(s);
    int    length = SCH_LENGTH(x);

    return INT2FIX( length );   /* fixnumじゃ溢れるかもしれない */
}

SchObj subr_is_list ( int s, int n )
{
    SchObj x = POP_STACK(s);
    return is_list(x) ? SCH_TRUE : SCH_FALSE;
}

SchObj subr_append( int s, int n )
{
    int n1 = n - 1;
    int s1 = s;
    SchObj ret;
    SchObj lst = SCH_NIL;
    SchObj last = INDEX_ST(s, n-1);
    SchObj lc = SCH_NIL;
    SchObj tc;
    SchObj kar;
    SchObj SCH_MANGLE(tmp);

    LOOP_STACK(s1,n1,lst){
        if(PAIRP(lst)){
            tc = SCH_CONS(SCH_CAR(lst),SCH_NIL);
            if ( lc != SCH_NIL ) {
                SCH_SET_CDR(lc,tc);
            } else {
                ret = tc;
            }
            lc = tc;
            lst = SCH_CDR(lst);
            FOR_EACH(kar,lst){
                tc = SCH_CONS(kar,SCH_NIL);
                SCH_SET_CDR(lc,tc);
                lc = tc;
            }
        }
    }

    if ( lc != SCH_NIL ) {
        SCH_SET_CDR(lc,last);
    } else {
        ret = last;
    }

/*     int    n1  = n-1; */
/*     int    s1  = s; */
/*     SchObj x; */
/*     SchObj lc; */
/*     SchObj ret = INDEX_ST(s, n-1); */

/*     LOOP_STACK_REV(s1,n1,x){ */
/*         if ( ! LISTP(x) ) { */
/*             EXCEPTION("list required."); */
/*         } else { */
/*             lc = last_cons(x); */
/*             SCH_SET_CDR(lc,ret); */
/*             ret = x; */
/*         } */
/*     } */

    return ret;
}

SchObj subr_memq( int s, int n )
{
    SchObj x   = POP_STACK(s);
    SchObj lst = POP_STACK(s);

    while ( PAIRP(lst) ) {
        if ( subr_is_eq_impl( SCH_CAR(lst), x ) ) {
            return lst;
        }
        lst = SCH_CDR(lst);
    }
    return SCH_FALSE;
}
SchObj subr_memv( int s, int n )
{
    SchObj x   = POP_STACK(s);
    SchObj lst = POP_STACK(s);

    while ( PAIRP(lst) ) {
        if ( subr_is_eqv_impl( SCH_CAR(lst), x ) ) {
            return lst;
        }
        lst = SCH_CDR(lst);
    }
    return SCH_FALSE;
}
SchObj subr_member( int s, int n )
{
    SchObj x   = POP_STACK(s);
    SchObj lst = POP_STACK(s);

    while ( PAIRP(lst) ) {
        if ( subr_is_equal_impl( SCH_CAR(lst), x ) ) {
            return lst;
        }
        lst = SCH_CDR(lst);
    }
    return SCH_FALSE;
}
SchObj subr_assq( int s, int n )
{
    SchObj x    = POP_STACK(s);
    SchObj alst = POP_STACK(s);
    SchObj kar;
    SchObj SCH_MANGLE(tmp);

    FOR_EACH(kar,alst){
        if ( PAIRP(kar) && subr_is_eq_impl( x, SCH_CAR(kar) ) ) {
            return kar;
        }
    }
    return SCH_FALSE;
}
SchObj subr_assv( int s, int n )
{
    SchObj x    = POP_STACK(s);
    SchObj alst = POP_STACK(s);
    SchObj kar;
    SchObj SCH_MANGLE(tmp);

    FOR_EACH(kar,alst){
        if ( PAIRP(kar) && subr_is_eqv_impl( x, SCH_CAR(kar) ) ) {
            return kar;
        }
    }
    return SCH_FALSE;
}
SchObj subr_assoc( int s, int n )
{
    SchObj x    = POP_STACK(s);
    SchObj alst = POP_STACK(s);
    SchObj kar;
    SchObj SCH_MANGLE(tmp);

    FOR_EACH(kar,alst){
        if ( PAIRP(kar) && subr_is_equal_impl( x, SCH_CAR(kar) ) ) {
            return kar;
        }
    }
    return SCH_FALSE;
}
SchObj subr_caar(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CAAR(x);
}
SchObj subr_cadr(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CADR(x);
}
SchObj subr_cdar(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CDAR(x);
}
SchObj subr_cddr(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CDDR(x);
}
SchObj subr_cadar(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CADAR(x);
}
SchObj subr_caddr(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CADDR(x);
}
SchObj subr_cdddr(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CDDDR(x);
}
SchObj subr_cadddr(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CADDDR(x);
}
SchObj subr_cddddr(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CDDDDR(x);
}
SchObj subr_caddddr(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CAR(SCH_CDDDDR(x));
}
SchObj subr_caaaar(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CAAAAR(x);
}
SchObj subr_caaadr(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CAAADR(x);
}
SchObj subr_caaar(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CAAAR(x);
}
SchObj subr_caadar(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CAADAR(x);
}
SchObj subr_caaddr(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CAADDR(x);
}
SchObj subr_caadr(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CAADR(x);
}
SchObj subr_cadaar(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CADAAR(x);
}
SchObj subr_cadadr(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CADADR(x);
}
SchObj subr_caddar(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CADDAR(x);
}
SchObj subr_cdaaar(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CDAAAR(x);
}
SchObj subr_cdaadr(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CDAADR(x);
}
SchObj subr_cdaar(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CDAAR(x);
}
SchObj subr_cdadar(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CDADAR(x);
}
SchObj subr_cdaddr(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CDADDR(x);
}
SchObj subr_cdadr(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CDADR(x);
}
SchObj subr_cddaar(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CDDAAR(x);
}
SchObj subr_cddadr(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CDDADR(x);
}
SchObj subr_cddar(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CDDAR(x);
}
SchObj subr_cdddar(int s, int n)
{
    SchObj x = POP_STACK(s);
    return SCH_CDDDAR(x);
}

/* ------ symbol --------------------------------------------------------  */
SchObj subr_is_symbol ( int s, int n )
{
    SchObj x = POP_STACK(s);
    return ( SYMBOLP(x) ? SCH_TRUE : SCH_FALSE );
}

SchObj subr_symbol2string ( int s, int n )
{
    SchObj x = POP_STACK(s);
    return SCH_STRING((SCH_ID2NAME(x)));
}

SchObj subr_string2symbol( int s, int n )
{
    SchObj x = POP_STACK(s);
    return SCH_SYMBOL(SCH_STRING_OBJ(x)->buf);
}

/* ------ char ----------------------------------------------------------- */
SchObj subr_is_char( int s, int n )
{
    SchObj x = POP_STACK(s);
    return ( CHARP(x) ? SCH_TRUE : SCH_FALSE );
}

SchObj subr_char_is_equal( int s, int n )
{
    SchObj x, y;
    x = POP_STACK(s);
    y = POP_STACK(s);
    return CHAR_EQUALP(x,y);
}

SchObj subr_char_is_equal_ci( int s, int n )
{
    SchObj x, y;
    x = POP_STACK(s);
    y = POP_STACK(s);

    if ( SCH_CHAR_OBJ(x)->ch == SCH_CHAR_OBJ(y)->ch ) {
        return SCH_TRUE;
    } else if ( tolower(SCH_CHAR_OBJ(x)->ch) == tolower(SCH_CHAR_OBJ(y)->ch) ) {
        return SCH_TRUE;
    } else {
        return SCH_FALSE;
    }
}

SchObj subr_char_is_greater( int s, int n)
{
    SchObj x, y;
    x = POP_STACK(s);
    y = POP_STACK(s);
    return ( ((SCH_CHAR_OBJ(x)->ch) < (SCH_CHAR_OBJ(y)->ch) ) ? SCH_TRUE : SCH_FALSE );
}

SchObj subr_char_is_greater_ci( int s, int n)
{
    SchObj x, y;
    x = POP_STACK(s);
    y = POP_STACK(s);

    if ( tolower(SCH_CHAR_OBJ(x)->ch) < tolower(SCH_CHAR_OBJ(y)->ch) ) {
        return SCH_TRUE;
    } else {
        return SCH_FALSE;
    }
}

SchObj subr_char_is_less( int s, int n)
{
    SchObj x, y;
    x = POP_STACK(s);
    y = POP_STACK(s);
    return ( ((SCH_CHAR_OBJ(x)->ch) > (SCH_CHAR_OBJ(y)->ch) ) ? SCH_TRUE : SCH_FALSE );
}

SchObj subr_char_is_less_ci( int s, int n)
{
    SchObj x, y;
    x = POP_STACK(s);
    y = POP_STACK(s);
    if ( tolower(SCH_CHAR_OBJ(x)->ch) > tolower(SCH_CHAR_OBJ(y)->ch) ) {
        return SCH_TRUE;
    } else {
        return SCH_FALSE;
    }
}

SchObj subr_char_is_equal_to_or_greater( int s, int n)
{
    SchObj x, y;
    x = POP_STACK(s);
    y = POP_STACK(s);
    return ( ((SCH_CHAR_OBJ(x)->ch) <= (SCH_CHAR_OBJ(y)->ch) ) ? SCH_TRUE : SCH_FALSE );
}

SchObj subr_char_is_equal_to_or_greater_ci( int s, int n)
{
    SchObj x, y;
    x = POP_STACK(s);
    y = POP_STACK(s);
    if ( tolower(SCH_CHAR_OBJ(x)->ch) <= tolower(SCH_CHAR_OBJ(y)->ch) ){
        return SCH_TRUE;
    } else {
        return SCH_FALSE;
    }
}

SchObj subr_char_is_equal_to_or_less( int s, int n)
{
    SchObj x, y;
    x = POP_STACK(s);
    y = POP_STACK(s);
    return ( ((SCH_CHAR_OBJ(x)->ch) >= (SCH_CHAR_OBJ(y)->ch) ) ? SCH_TRUE : SCH_FALSE );
}

SchObj subr_char_is_equal_to_or_less_ci( int s, int n)
{
    SchObj x, y;
    x = POP_STACK(s);
    y = POP_STACK(s);
    if ( tolower(SCH_CHAR_OBJ(x)->ch) >= tolower(SCH_CHAR_OBJ(y)->ch) ) {
        return SCH_TRUE;
    } else {
        return SCH_FALSE;
    }
}

SchObj subr_char2integer(int s, int n)
{
    SchObj x;
    x = POP_STACK(s);
    return INT2FIX( SCH_CHAR_OBJ(x)->ch );
}

SchObj subr_integer2char(int s, int n)
{
    SchObj x;
    x = POP_STACK(s);
    return SCH_CHAR((FIX2INT(x)));
}

SchObj subr_char_is_alphabetic(int s, int n)
{
    SchObj       x    = POP_STACK(s);
    unsigned int ch   = SCH_CHAR_OBJ(x)->ch;

/*     int          size = get_char_byte_size(ch); */
/*     char         c; */
/*     if (size != 1) return SCH_FALSE; */
/*     c = (0xffU & ch); */

    if ( isalpha(ch) ) {
        return SCH_TRUE;
    } else {
        return SCH_FALSE;
    }

}

SchObj subr_char_is_whitespace(int s, int n)
{
    SchObj       x    = POP_STACK(s);
    unsigned int ch   = SCH_CHAR_OBJ(x)->ch;

    if ( isspace(ch) ) {
        return SCH_TRUE;
    } else {
        return SCH_FALSE;
    }
}

SchObj subr_char_is_lower_case(int s, int n)
{
    SchObj       x    = POP_STACK(s);
    unsigned int ch   = SCH_CHAR_OBJ(x)->ch;

    if ( islower(ch) ) {
        return SCH_TRUE;
    } else {
        return SCH_FALSE;
    }
}

SchObj subr_char_is_upper_case(int s, int n)
{
    SchObj       x    = POP_STACK(s);
    unsigned int ch   = SCH_CHAR_OBJ(x)->ch;

    if ( isupper(ch) ) {
        return SCH_TRUE;
    } else {
        return SCH_FALSE;
    }
}

SchObj subr_char_is_numeric(int s, int n)
{
    SchObj       x    = POP_STACK(s);
    unsigned int ch   = SCH_CHAR_OBJ(x)->ch;

    if ( isdigit(ch) ) {
        return SCH_TRUE;
    } else {
        return SCH_FALSE;
    }
}

SchObj subr_char_upcase(int s, int n)
{
    SchObj        x  = POP_STACK(s);
    unsigned int  ch = SCH_CHAR_OBJ(x)->ch;
    unsigned char c  = (unsigned char)toupper(ch);
    return SCH_CHAR(c);
}
SchObj subr_char_downcase(int s, int n)
{
    SchObj        x  = POP_STACK(s);
    unsigned int  ch = SCH_CHAR_OBJ(x)->ch;
    unsigned char c  = (unsigned char)tolower(ch);
    return SCH_CHAR(c);
}

/* ------ string ----------------------------------------------------------- */
SchObj subr_is_string(int s, int n)
{
    SchObj x;
    x = POP_STACK(s);
    return ( STRINGP(x) ? SCH_TRUE : SCH_FALSE );
}

SchObj subr_string_length(int s, int n)
{
    SchObj x;
    x = POP_STACK(s);
    return INT2FIX(SCH_STRING_OBJ(x)->len);
}

SchObj subr_make_string_k_ch(int s, int n)
{
    SchObj x;
    SchObj c;
    x = POP_STACK(s);
    c = POP_STACK(s);
    return make_string_k_ch(FIX2INT(x),SCH_CHAR_OBJ(c)->ch);
}

SchObj subr_make_string_k(int s, int n)
{
    SchObj x;

    if ( n == 2 ) {
        return subr_make_string_k_ch(s,n);
    }

    x = POP_STACK(s);
    return ( make_string_k(FIX2INT(x)));
}

SchObj subr_string(int s, int n)
{
    SchObj x,ret;
    unsigned int ch;

    ret = make_string_k(n);
    char* buf = SCH_STRING_OBJ(ret)->buf;

    while ( n > 0 ) {
        x    = POP_STACK(s);
        ch   = SCH_CHAR_OBJ(x)->ch;
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
        n--;
    }
    *buf = '\0';

    return ret;
}

SchObj subr_string_ref( int s, int n )
{
    SchObj x;
    int    i;
    x = POP_STACK(s);
    i = FIX2INT(POP_STACK(s));
    return string_ref(SCH_STRING_OBJ(x),i);
}

SchObj subr_string_set( int s, int n)
{
    SchObj x;
    SchObj i;
    SchObj c;
    x = POP_STACK(s);
    i = POP_STACK(s);
    c = POP_STACK(s);
    string_set( SCH_STRING_OBJ(x),
                FIX2INT(i),
                SCH_CHAR_OBJ(c)->ch );
    return SCH_UNDEFINE;
}

int subr_string_is_equal_impl( SchObj x, SchObj y )
{
    return ( strcmp( SCH_STRING_OBJ(x)->buf,
                     SCH_STRING_OBJ(y)->buf ) == 0 );
}
int str_cmp_ci( int s, int n )
{
    SchObj x = POP_STACK(s);
    SchObj y = POP_STACK(s);
    char *cx = SCH_STRING_OBJ(x)->buf;
    char *cy = SCH_STRING_OBJ(y)->buf;
    char cxl,cyl;

    while ( (*cx != '\0') || (*cy != '\0') ) {
        if ( (cxl = tolower((int)*cx)) != (cyl = tolower((int)*cy)) ) {
            return (cxl < cyl) ? -1 : 1 ;
        }
        cx++;
        cy++;
    }

    return 0;
}
SchObj subr_string_is_equal( int s, int n )
{
    SchObj x = POP_STACK(s);
    SchObj y = POP_STACK(s);
    return ( subr_string_is_equal_impl(x,y) ? SCH_TRUE : SCH_FALSE );
}
SchObj subr_string_is_equal_ci( int s, int n )
{
    return (str_cmp_ci(s,n) == 0) ? SCH_TRUE : SCH_FALSE;
}
int str_cmp(int s, int n)
{
    SchObj x = POP_STACK(s);
    SchObj y = POP_STACK(s);
    return strcmp( SCH_STRING_OBJ(x)->buf, SCH_STRING_OBJ(y)->buf );
}
SchObj subr_string_is_greater( int s, int n)
{
    return ( str_cmp(s,n) < 0 ) ? SCH_TRUE : SCH_FALSE;
}
SchObj subr_string_is_greater_ci( int s, int n)
{
    return ( str_cmp_ci(s,n) < 0 ) ? SCH_TRUE : SCH_FALSE;
}
SchObj subr_string_is_less( int s, int n)
{
    return ( str_cmp(s,n) > 0 ) ? SCH_TRUE : SCH_FALSE;
}
SchObj subr_string_is_less_ci( int s, int n)
{
    return ( str_cmp_ci(s,n) > 0 ) ? SCH_TRUE : SCH_FALSE;
}
SchObj subr_string_is_equal_or_greater( int s, int n)
{
    return ( str_cmp(s,n) <= 0 ) ? SCH_TRUE : SCH_FALSE;
}
SchObj subr_string_is_equal_or_less( int s, int n)
{
    return ( str_cmp(s,n) >= 0 ) ? SCH_TRUE : SCH_FALSE;
}
SchObj subr_string_is_equal_or_greater_ci( int s, int n)
{
    return ( str_cmp_ci(s,n) <= 0 ) ? SCH_TRUE : SCH_FALSE;
}
SchObj subr_string_is_equal_or_less_ci( int s, int n)
{
    return ( str_cmp_ci(s,n) >= 0 ) ? SCH_TRUE : SCH_FALSE;
}
SchObj subr_string_append( int s, int n )
{
    SchObj x;
    SchObj ret;
    int s1,s2,n1,n2;
    int len = 0;

    s1 = s2 = s;
    n1 = n2 = n;

    LOOP_STACK(s1,n1,x) {
        len += strlen(SCH_STRING_OBJ(x)->buf);
    }

    ret = make_string_k(len);

    LOOP_STACK(s2,n2,x) {
        strcat( SCH_STRING_OBJ(ret)->buf,
                SCH_STRING_OBJ(x)->buf );
    }
    return ret;
}
SchObj subr_substring( int s, int n )
{
    SchObj str;
    int beg, end;

    str = POP_STACK(s);
    beg = FIX2INT(POP_STACK(s));
    end = FIX2INT(POP_STACK(s));

    return substring(str,beg,end);
}
SchObj subr_string_copy( int s, int n )
{
    SchObj str = POP_STACK(s);
    return make_string(SCH_STRING_OBJ(str)->buf);
}
SchObj subr_string2list ( int s, int n )
{
    SchObj str = POP_STACK(s);
    return string2list(str);
}
SchObj subr_list2string( int s, int n )
{
    SchObj lst = POP_STACK(s);
    return list2string(lst);
}
SchObj subr_string_fill( int s, int n )
{
    SchObj str = POP_STACK(s);
    SchObj ch  = POP_STACK(s);
    string_fill(str,ch);

    return SCH_UNDEFINE;
}
/* ------ vector --------------------------------------------------------------- */
SchObj subr_is_vector( int s, int n)
{
    SchObj x;
    x = POP_STACK(s);
    return ( VECTORP(x) ? SCH_TRUE : SCH_FALSE );
}

SchObj subr_make_vector_fill( int s, int n)
{
    SchObj x;
    SchObj y;
    x = POP_STACK(s);
    y = POP_STACK(s);
    return SCH_VECTOR_F(FIX2INT(x),y);
}

SchObj subr_make_vector( int s, int n)
{
    SchObj x;

    if ( n == 2 ) return subr_make_vector_fill(s,n);

    x = POP_STACK(s);
    return SCH_VECTOR(FIX2INT(x));
}

SchObj subr_vector_length( int s, int n )
{
    SchObj v;
    v = POP_STACK(s);
    return INT2FIX(SCH_VECTOR_LEN(v));
}

SchObj subr_vector_ref( int s, int n)
{
    SchObj v;
    SchObj k;
    int i;
    v = POP_STACK(s);
    k = POP_STACK(s);
    i = FIX2INT(k);
    return SCH_VECTOR_REF(v,i);
}

SchObj subr_vector_set( int s , int n )
{
    SchObj v;
    SchObj k;
    SchObj x;
    int i;
    v = POP_STACK(s);
    k = POP_STACK(s);
    x = POP_STACK(s);
    i = FIX2INT(k);
    SCH_VECTOR_OBJ(v)->vec[i] = x;
    return SCH_UNDEFINE;
}

SchObj subr_vector_fill(int s, int n)
{
    SchObj vec = POP_STACK(s);
    SchObj obj = POP_STACK(s);

    return vector_fill(vec,obj);
}

SchObj subr_vector2list(int s, int n)
{
    SchObj vec = POP_STACK(s);
    return vector2list(vec);
}

/* ------ procedure ------------------------------------------------------------- */
SchObj subr_is_procedure ( int s, int n )
{
    SchObj x = POP_STACK(s);
    return ( SUBRP(x) || LAMBDAP(x) ) ? SCH_TRUE : SCH_FALSE;
}
/* ------ apply ----------------------------------------------------------------- */
int shift_up_args(int n,int m,int s)
{
    int i;
    for ( i = 1 ; i <= n ; ++i ) {
        stack[s - i + m] = stack[s - i];
#ifdef _VM_DEBUG
        stack_att[s - i + m] = stack_att[s - i];
#endif
    }
    return s + m;
}

SchObj subr_subr_apply( SchObj f, int s, int n )
{
    SchObj x;
    SchObj kar;
    int    sp;
    int    c;
    SchObj SCH_MANGLE(tmp);

    sp = s;
    c  = 0;

    LOOP_STACK_REV(s,n,x){
        if ( PAIRP(x) ) {
            SchObj lst;
            SCH_STACK_INIT(lst);
            FOR_EACH(kar,x){
                SCH_STACK_PUSH(lst,kar);
            }
            FOR_EACH(kar,lst){
                sp = push_stk(kar,sp);
                c++;
            }
        } else {
            sp = push_stk(x,sp);
            c++;
        }
    }
    return (*(SCH_CLOSURE_OBJ(f)->u.subr))(sp, c);
}

SchObj subr_lambda_apply(SchObj f, int s, int n)
{
    DisplayClosure* cl;
    SchObj* top;

    SchObj lst;
    int    len;
    int    sp;
    int    i;

    cl   = SCH_CLOSURE_OBJ(f);
    top  = cl->u.body.body_top;

    lst = INDEX_ST( s, n-1 );
    len = SCH_LENGTH( lst );
    sp  = shift_up_args( n-1, len-1, s );

    for ( i=0 ; i<len ; ++i ) {
        SchObj obj = SCH_CAR( lst );
        INDEX_SET( s,
                   (n-1)-(len-1)+i,
                   obj );
        lst = SCH_CDR( lst );
    }

    sp = shift_up_args( n, 3, sp);

    /* insert a dummy frame */
    INDEX_SET(sp, n,   &HALT);
    INDEX_SET(sp, n+1, 0);
    INDEX_SET(sp, n+2, 0);
#ifdef _VM_DEBUG
    INDEX_SET_ATT(sp,n,  3);
    INDEX_SET_ATT(sp,n+1,1);
    INDEX_SET_ATT(sp,n+2,1);
#endif

    SchObj ret = vm(0,top,cl,sp,sp);

    return ret;
}

SchObj subr_apply( int s, int n )
{
    SchObj f;

    f = POP_STACK(s); n--;
    if ( SUBRP(f) ) {
        return subr_subr_apply(f,s,n);
    } else {
        return subr_lambda_apply(f,s,n);
    }
}

SchObj subr_map( int s, int n)
{
    int    sp = s;
    SchObj fn = POP_STACK(sp);
    SchObj *args;
    size_t arg_num = n-1;
    int    i;
    SchObj ret;
    SchObj SCH_MANGLE(tmp);

    args = SCH_MALLOC( sizeof(SchObj) * (n-1) );

    for ( i=0 ; i<arg_num ; ++i ) {
        args[i] = POP_STACK(sp);
    }

    SCH_QUEUE_INIT(ret);

    while ( 1 ) {
        SchObj arg;
        sp = s;

        i = arg_num-1;
        if ( SCH_NIL == args[i] ) {
            return ret;
        }
        arg = SCH_LIST1( SCH_CAR(args[i]) );
        sp = push_stk( arg, sp );
#ifdef _VM_DEBUG
        sp = push_att_obj( --sp );
#endif
        args[i] = SCH_CDR( args[i] );
        
        for ( i = arg_num-2 ; i >= 0 ; --i ) {
            if ( SCH_NIL == args[i] ) {
                return ret;
            }
            arg = SCH_CAR( args[i] );
            sp  = push_stk( arg, sp );
#ifdef _VM_DEBUG
            sp = push_att_obj( --sp );
#endif
            args[i] = SCH_CDR( args[i] );
        }

        sp = push_stk( fn, sp );
#ifdef _VM_DEBUG
        sp = push_att_obj( --sp );
#endif

        SCH_QUEUE_PUSH( ret,
                        subr_apply( sp, n ) );
    }

    return ret;
}

SchObj subr_foreach( int s, int n )
{
    int    sp = s;
    SchObj fn = POP_STACK(sp);
    SchObj *args;
    size_t arg_num = n-1;
    int    i;
/*     SchObj SCH_MANGLE(tmp); */

    args = SCH_MALLOC( sizeof(SchObj) * arg_num );

    for ( i=0 ; i<arg_num ; ++i ) {
        args[i] = POP_STACK(sp);
    }

    while ( 1 ) {
        SchObj arg;
        sp = s;

        i = arg_num-1;
        if ( SCH_NIL == args[i] ) {
            return SCH_UNDEFINE;
        }
        arg = SCH_LIST1( SCH_CAR(args[i]) );
        sp = push_stk( arg, sp );
#ifdef _VM_DEBUG
        sp = push_att_obj( --sp );
#endif
        args[i] = SCH_CDR( args[i] );
        
        for ( i = arg_num-2 ; i >= 0 ; --i ) {
            if ( SCH_NIL == args[i] ) {
                return SCH_UNDEFINE;
            }
            arg = SCH_CAR( args[i] );
            sp  = push_stk( arg, sp );
#ifdef _VM_DEBUG
            sp = push_att_obj( --sp );
#endif
            args[i] = SCH_CDR( args[i] );
        }

        sp = push_stk( fn, sp );
#ifdef _VM_DEBUG
        sp = push_att_obj( --sp );
#endif
        subr_apply( sp, n );
    }

    return SCH_UNDEFINE;
}

SchObj subr_list_ref( int s, int n )
{
    SchObj lst   = POP_STACK(s);
    SchObj index = POP_STACK(s);
    return list_ref(lst,FIX2INT(index));
}
SchObj subr_list_tail(int s, int n)
{
    SchObj lst   = POP_STACK(s);
    SchObj index = POP_STACK(s);
    return list_tail(lst,FIX2INT(index));
}
SchObj subr_reverse(int s, int n)
{
    SchObj lst = POP_STACK(s);
    return reverse(lst);
}
SchObj subr_list2vector(int s, int n)
{
    SchObj lst = POP_STACK(s);
    return list2vector(lst);
}
/* /\*------------------------------------------ I/O -------------------------------------------------------*\/ */
SchObj subr_is_input_port(int s, int n)
{
    SchObj p;
    p = POP_STACK(s);
    return ((is_input_port(SCH_PORT_OBJ(p))) ? SCH_TRUE : SCH_FALSE) ;
}
SchObj subr_is_output_port(int s, int n)
{
    SchObj p;
    p = POP_STACK(s);
    return ((is_output_port(SCH_PORT_OBJ(p))) ? SCH_TRUE : SCH_FALSE);
}
SchObj subr_open_input_file(int s, int n)
{
    SchObj f;
    f = POP_STACK(s);
    return open_input_file(SCH_STRING_OBJ(f)->buf);
}
SchObj subr_open_output_file(int s, int n)
{
    SchObj f;
    f = POP_STACK(s);
    return open_output_file(SCH_STRING_OBJ(f)->buf);
}
SchObj subr_close_input_port(int s, int n)
{
    SchObj p;
    p = POP_STACK(s);
    close_port(SCH_PORT_OBJ(p));
    return SCH_UNDEFINE;
}
SchObj subr_close_output_port(int s, int n)
{
    SchObj p;
    p = POP_STACK(s);
    close_port(SCH_PORT_OBJ(p));
    return SCH_UNDEFINE;
}

SchObj subr_write(int s, int n)
{
    SchObj obj = POP_STACK(s);
    SchPort* port;

    if ( n == 2 ) {
        port = SCH_PORT_OBJ(POP_STACK(s));
    } else if (n == 1) {
        port = current_output_port();
    } else {
        EXCEPTION("wrong number of arguments");
        return SCH_NIL;
    }
    sch_write(port->src.stream.fp,obj);
    return SCH_UNDEFINE;
}
SchObj subr_display(int s, int n)
{
    SchObj obj = POP_STACK(s);
    SchPort* port;

    if ( n == 2 ) {
        port = SCH_PORT_OBJ(POP_STACK(s));
    } else if (n == 1) {
        port = current_output_port();
    } else {
        EXCEPTION("wrong number of arguments");
        return SCH_NIL;
    }
    sch_display(port->src.stream.fp,obj);
    return SCH_UNDEFINE;
}

SchObj subr_read_char(int s, int n)
{
    unsigned int ch;
    SchPort* port = current_input_port();
    if ( n == 1 ){
        port = SCH_PORT_OBJ(POP_STACK(s));
    }
    ch = read_char(port);
    return SCH_CHAR(ch);
}

SchObj subr_write_char(int s, int n)
{
    SchPort* port;
    SchObj   obj = POP_STACK(s);
    if ( n == 1 ) {
        port = current_output_port();
    } else if ( n == 2 ) {
        port = SCH_PORT_OBJ(POP_STACK(s));
    } else {
        EXCEPTION("wrong number of arguments");
        return SCH_NIL;
    }
    display_char(port,obj);
    return SCH_UNDEFINE;
}

SchObj subr_newline(int s, int n)
{
    SchPort* port;
    if ( n == 1 ) {
        port = SCH_PORT_OBJ(POP_STACK(s));
    } else if ( n == 0 ){
        port = current_output_port();
    } else {
        EXCEPTION("wrong number of arguments");
        return SCH_NIL;
    }
    sch_newline(port);
    return SCH_UNDEFINE;
}

SchObj subr_read(int s, int n)
{
    SchPort* port;
    if ( n == 1 ) {
        port = SCH_PORT_OBJ(POP_STACK(s));
    } else if ( n == 0 ){
        port = current_input_port();
    } else {
        EXCEPTION("wrong number of arguments");
        return SCH_NIL;
    }

    return read_obj_via_port( SCH_PORT_OBJ(port) );
}

SchObj subr_standard_input_port(int s, int n)
{
    return (SchObj)sch_stdin;
}

SchObj subr_standard_output_port(int s, int n)
{
    return (SchObj)sch_stdout;
}

SchObj subr_current_input_port(int s, int n)
{
    SchPort* ret = current_input_port();
    if ( n != 0 ) {
        SchPort* port = SCH_PORT_OBJ(POP_STACK(s));
        set_current_input_port(port);
    }
    return (SchObj)ret;
}

SchObj subr_current_output_port(int s, int n)
{
    SchPort* ret = current_output_port();
    if ( n != 0 ) {
        SchPort* port = SCH_PORT_OBJ(POP_STACK(s));
        set_current_output_port(port);
    }
    return (SchObj)ret;
}

/* TODO currentだけじゃなくload-pathからもファイルを探してくるようにする */
SchObj sch_load(SchString* filename)
{
    SchPort * port = SCH_PORT_OBJ(open_input_file(filename->buf));
    SchObj    sexp;
    SchObj    rslt;

    while ( SCH_EOF != (sexp = read_obj_via_port(port)) ) {
/*         SCH_WRITE(sexp); */
/*         SCH_DISPLAY(SCH_CHAR('\n')); */
        rslt = vm_compile(sexp);
/*         SCH_WRITE(rslt); */
/*         SCH_DISPLAY(SCH_CHAR('\n')); */
    }

    return SCH_UNDEFINE;
}
SchObj subr_load(int s, int n)
{
    SchObj filename = POP_STACK(s);
    return sch_load(SCH_STRING_OBJ(filename));
}

SchObj subr_is_eof_object(int s, int n)
{
    SchObj obj = POP_STACK(s);
    return EOFP(obj) ? SCH_TRUE : SCH_FALSE;
}






DisplayClosure* make_subrs( SubrPnt* pnt, char** name )
{
    int i = 0;
    pnt[i] = subr_mul;        name[i++] = "*";    /* 0 */
    pnt[i] = subr_add;        name[i++] = "+";
    pnt[i] = subr_sub;        name[i++] = "-";
    pnt[i] = subr_div;        name[i++] = "/";
    pnt[i] = subr_is_greater; name[i++] = "<";
    pnt[i] = subr_is_less;    name[i++] = ">";
    pnt[i] = subr_is_zero;    name[i++] = "zero?";
    pnt[i] = subr_is_equal_to_or_greater; name[i++] = "<=";
    pnt[i] = subr_is_equal_to_or_less;    name[i++] = ">=";
    pnt[i] = subr_is_equal_num;    name[i++] = "=";
    pnt[i] = subr_not;             name[i++] = "not";        /* 10 */
    pnt[i] = subr_is_boolean;      name[i++] = "boolean?";
    pnt[i] = subr_is_pair;         name[i++] = "pair?";
    pnt[i] = subr_cons;            name[i++] = "cons";
    pnt[i] = subr_car;             name[i++] = "car";
    pnt[i] = subr_cdr;             name[i++] = "cdr";
    pnt[i] = subr_set_car;         name[i++] = "set-car!";
    pnt[i] = subr_set_cdr;         name[i++] = "set-cdr!";
    pnt[i] = subr_is_null;         name[i++] = "null?";
    pnt[i] = subr_list;            name[i++] = "list";
    pnt[i] = subr_length;          name[i++] = "length";     /* 20 */
    pnt[i] = subr_is_symbol;       name[i++] = "symbol";
    pnt[i] = subr_symbol2string;   name[i++] = "symbol->string";
    pnt[i] = subr_string2symbol;   name[i++] = "string->symbol";
    pnt[i] = subr_is_char;         name[i++] = "char?";
    pnt[i] = subr_char_is_equal;   name[i++] = "char=?";
    pnt[i] = subr_char_is_greater; name[i++] = "char<?";
    pnt[i] = subr_char_is_less;    name[i++] = "char>?";
    pnt[i] = subr_char_is_equal_to_or_greater;  name[i++] = "char<=?";
    pnt[i] = subr_char_is_equal_to_or_less;     name[i++] = "char>=?";
    pnt[i] = subr_char2integer;    name[i++] = "char->integer";/* 30 */
    pnt[i] = subr_integer2char;    name[i++] = "integer->char";
    pnt[i] = subr_is_string;       name[i++] = "string?";
    pnt[i] = subr_string_length;   name[i++] = "string-length";
    pnt[i] = subr_make_string_k;   name[i++] = "make-string";
    pnt[i] = subr_string_ref;      name[i++] = "string-ref";
    pnt[i] = subr_string_set;      name[i++] = "string-set!";
    pnt[i] = subr_is_vector;       name[i++] = "vector?";
    pnt[i] = subr_make_vector;     name[i++] = "make-vector";
    pnt[i] = subr_vector_length;   name[i++] = "vector-length";
    pnt[i] = subr_vector_ref;      name[i++] = "vector-ref"; /* 40 */
    pnt[i] = subr_vector_set;      name[i++] = "vector-set!";
    pnt[i] = subr_number2string;   name[i++] = "number->string";
    pnt[i] = subr_is_eq;           name[i++] = "eq?";
    pnt[i] = subr_is_eqv;          name[i++] = "eqv?";
    pnt[i] = subr_is_equal;        name[i++] = "equal?";
    pnt[i] = subr_is_number;       name[i++] = "number?";
    pnt[i] = subr_is_procedure;    name[i++] = "procedure?";
    pnt[i] = subr_apply;      name[i++] = "apply";
    pnt[i] = subr_is_list;    name[i++] = "list?";
    pnt[i] = subr_append;     name[i++] = "append";    /* 50 */
    pnt[i] = subr_memq;       name[i++] = "memq";
    pnt[i] = subr_memv;       name[i++] = "memv";
    pnt[i] = subr_assq;       name[i++] = "assq";
    pnt[i] = subr_assv;       name[i++] = "assv";
    pnt[i] = subr_assoc;      name[i++] = "assoc";
    pnt[i] = subr_caar;       name[i++] = "caar";
    pnt[i] = subr_cadar;      name[i++] = "cadar";
    pnt[i] = subr_cadddr;     name[i++] = "cadddr";
    pnt[i] = subr_caddr;      name[i++] = "caddr";
    pnt[i] = subr_cadr;       name[i++] = "cadr";   /* 60 */
    pnt[i] = subr_cdar;       name[i++] = "cdar";
    pnt[i] = subr_cdddr;      name[i++] = "cdddr";
    pnt[i] = subr_cddddr;     name[i++] = "cddddr";
    pnt[i] = subr_cddr;       name[i++] = "cddr";
    pnt[i] = subr_string_is_equal; name[i++] = "string=?";
    pnt[i] = subr_string_append;   name[i++] = "string-append";
    pnt[i] = subr_substring;       name[i++] = "substring";
    pnt[i] = subr_map;             name[i++] = "map";
    pnt[i] = subr_list_ref;        name[i++] = "list-ref";
    pnt[i] = subr_list_tail;       name[i++] = "list-tail"; /* 70 */
    pnt[i] = subr_reverse;         name[i++] = "reverse";
    pnt[i] = subr_list2vector;     name[i++] = "list->vector";
    pnt[i] = subr_vector_fill;     name[i++] = "vector-fill!";
    pnt[i] = subr_vector2list;     name[i++] = "vector->list";
    pnt[i] = subr_string_copy;     name[i++] = "string-copy";
    pnt[i] = subr_caddddr;         name[i++] = "caddddr";
    pnt[i] = subr_caaaar;          name[i++] = "caaaar";
    pnt[i] = subr_caaadr;          name[i++] = "caaadr";
    pnt[i] = subr_caaar;           name[i++] = "caaar";
    pnt[i] = subr_caadar;          name[i++] = "caadar"; /* 80 */
    pnt[i] = subr_caaddr;          name[i++] = "caaddr";
    pnt[i] = subr_caadr;           name[i++] = "caadr";
    pnt[i] = subr_cadaar;          name[i++] = "cadaar";
    pnt[i] = subr_cadadr;          name[i++] = "cadadr";
    pnt[i] = subr_caddar;          name[i++] = "caddar";
    pnt[i] = subr_cdaaar;          name[i++] = "cdaaar";
    pnt[i] = subr_cdaadr;          name[i++] = "cdaadr";
    pnt[i] = subr_cdaar;           name[i++] = "cdaar";
    pnt[i] = subr_cdadar;          name[i++] = "cdadar";
    pnt[i] = subr_cdaddr;          name[i++] = "cdaddr"; /* 90 */
    pnt[i] = subr_cdadr;           name[i++] = "cdadr";
    pnt[i] = subr_cddaar;          name[i++] = "cddaar";
    pnt[i] = subr_cddadr;          name[i++] = "cddadr";
    pnt[i] = subr_cddar;           name[i++] = "cddar";
    pnt[i] = subr_cdddar;          name[i++] = "cdddar";
    pnt[i] = subr_member;          name[i++] = "member";
    pnt[i] = subr_vector;          name[i++] = "vector";
    pnt[i] = subr_is_input_port;   name[i++] = "input-port?";
    pnt[i] = subr_is_output_port;  name[i++] = "output-port?";
    pnt[i] = subr_open_input_file;  name[i++] = "open-input-file"; /* 100 */
    pnt[i] = subr_open_output_file; name[i++] = "open-output-file";
    pnt[i] = subr_close_input_port; name[i++] = "close-input-port";
    pnt[i] = subr_close_output_port;name[i++] = "close-output-port";
    pnt[i] = subr_write;            name[i++] = "write";
    pnt[i] = subr_display;          name[i++] = "display";
    pnt[i] = subr_read_char;        name[i++] = "read-char";
    pnt[i] = subr_write_char;       name[i++] = "write-char";
    pnt[i] = subr_newline;          name[i++] = "newline";
    pnt[i] = subr_read;             name[i++] = "read";
    pnt[i] = subr_foreach;          name[i++] = "for-each"; /* 110 */
    pnt[i] = subr_string2list;   name[i++] = "string->list";
    pnt[i] = subr_string;        name[i++] = "string";
    pnt[i] = subr_list2string;   name[i++] = "list->string";
    pnt[i] = subr_string_fill;   name[i++] = "string-fill!";
    pnt[i] = subr_char_is_alphabetic; name[i++] = "char-alphabetic?";
    pnt[i] = subr_char_is_whitespace; name[i++] = "char-whitespace?";
    pnt[i] = subr_char_is_lower_case; name[i++] = "char-lower-case?";
    pnt[i] = subr_char_is_upper_case; name[i++] = "char-upper-case?";
    pnt[i] = subr_char_is_numeric;    name[i++] = "char-numeric?";
    pnt[i] = subr_char_upcase;        name[i++] = "char-upcase"; /* 120 */
    pnt[i] = subr_char_downcase;      name[i++] = "char-downcase";
    pnt[i] = subr_char_is_equal_ci;   name[i++] = "char-ci=?";
    pnt[i] = subr_char_is_greater_ci; name[i++] = "char-ci<?";
    pnt[i] = subr_char_is_less_ci;    name[i++] = "char-ci>?";
    pnt[i] = subr_char_is_equal_to_or_greater_ci;  name[i++] = "char-ci<=?";
    pnt[i] = subr_char_is_equal_to_or_less_ci;     name[i++] = "char-ci>=?";
    pnt[i] = subr_string_is_greater; name[i++] = "string<?";
    pnt[i] = subr_string_is_less;    name[i++] = "string>?";
    pnt[i] = subr_string_is_equal_or_greater; name[i++] = "string<=?";
    pnt[i] = subr_string_is_equal_or_less;    name[i++] = "string>=?"; /* 130 */
    pnt[i] = subr_string_is_equal_ci;         name[i++] = "string-ci=?";
    pnt[i] = subr_string_is_greater_ci;       name[i++] = "string-ci<?";
    pnt[i] = subr_string_is_less_ci;    name[i++] = "string-ci>?";
    pnt[i] = subr_string_is_equal_or_greater_ci; name[i++] = "string-ci<=?";
    pnt[i] = subr_string_is_equal_or_less_ci;    name[i++] = "string-ci>=?";
    pnt[i] = subr_standard_input_port; name[i++] = "standard-input-port";
    pnt[i] = subr_standard_output_port; name[i++] = "standard-output-port";
    pnt[i] = subr_current_input_port;  name[i++] = "current-input-port";
    pnt[i] = subr_current_output_port; name[i++] = "current-output-port";
    pnt[i] = subr_load;                name[i++] = "load"; /* 140 */
    pnt[i] = subr_is_eof_object;       name[i++] = "eof-object?";
    pnt[i] = subr_gcd;                 name[i++] = "gcd";
    pnt[i] = subr_lcm;                 name[i++] = "lcm";
    pnt[i] = subr_is_even;             name[i++] = "even?";
    pnt[i] = subr_is_odd;              name[i++] = "odd?";
    pnt[i] = subr_is_integer;          name[i++] = "integer?";
    pnt[i] = subr_modulo;              name[i++] = "modulo";
    pnt[i] = subr_remainder;           name[i++] = "remainder";
    pnt[i] = subr_quotient;            name[i++] = "quotient";


    pnt[i] = NULL; name[i++] = NULL;

    return make_subrs_impl(pnt,name,NUM_OF_SUBR);
}

