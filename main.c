/*--------------------------------------------------------------------
  main.c -  MameScheme ( Scheme Interpreter based on R5RS )
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

#include <unistd.h>
#include "mame.h"


int flag_use_profiler;

int main ( int argc, char** argv )
{
    SchObj robj,ret;
    int    opt;
    char * fname = 0;
    char * expr = 0;

#ifdef USE_BOEHM_GC
    GC_INIT();
#endif
    ensure_init_symtable();
    init_reader();
    init_port();

    flag_use_profiler = 0;

    while ( (opt = getopt(argc,argv,"pf:e:")) != -1 ) {
        switch ( opt ) {
        case 'p': {
            flag_use_profiler = 1;
        } break;
        case 'f': {
            fname = SCH_MALLOC(strlen(optarg) + 1);
            strcpy(fname,optarg);
        } break;
        case 'e':{
            expr = SCH_MALLOC(strlen(optarg) + 1);
            strcpy(expr,optarg);
        }break;
        };
    }

    if ( expr ) {
        robj = sch_read_string(expr);
        ret  = vm_compile(robj);
        SCH_WRITE(ret);
        SCH_DISPLAY(SCH_CHAR('\n'));
    } else if ( fname ) {
        sch_load(SCH_STRING_OBJ(SCH_STRING(fname)));
    } else {
        while ( 1 ) {
            robj = sch_read(NULL);
            ret  = vm_compile(robj);
            SCH_WRITE(ret);
        }
    }

    return 0;

}


