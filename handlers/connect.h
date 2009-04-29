#ifndef _EMACSIM_HANDLER_CONNECT_H_
#define _EMACSIM_HANDLER_CONNECT_H_

#include "../elim-rpc.h"

xmlnode * _h_elim_connect ( const char *name , 
                            const char *id   ,
                            SEXP_VALUE *args , 
                            gpointer    data );

#endif
