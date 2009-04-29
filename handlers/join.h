#ifndef _EMACSIM_HANDLER_JOIN_H_
#define _EMACSIM_HANDLER_JOIN_H_

#include "../elim-rpc.h"

xmlnode * _h_elim_join ( const char *name , 
                         const char *id   ,
                         SEXP_VALUE *args , 
                         gpointer    data );

#endif
