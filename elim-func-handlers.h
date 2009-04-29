#ifndef _ELIM_FUNC_HANDLERS_H_
#define _ELIM_FUNC_HANDLERS_H_

#include "handler-list.h"
#include "sexp/sexp-util.h"

typedef xmlnode * (*sexp_func) ( const char *name , 
                                 const char *id   ,
                                 SEXP_VALUE *args , 
                                 gpointer    data );

typedef struct _func_handler func_handler;
struct _func_handler
{
    char      *name;
    sexp_func  func;
};

extern func_handler handlers[];

#endif
