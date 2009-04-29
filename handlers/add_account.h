#ifndef _EMACSIM_HANDLER_ADD_ACCOUNT_H_
#define _EMACSIM_HANDLER_ADD_ACCOUNT_H_

#include <glib.h>
#include "../elim-rpc.h"

xmlnode * _h_elim_add_account ( const char *name , 
                                const char *id   ,
                                SEXP_VALUE *args , 
                                gpointer    data );

#endif
