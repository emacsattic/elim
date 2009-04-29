#ifndef _EMACSIM_HANDLER_RESPONSE_H_
#define _EMACSIM_HANDLER_RESPONSE_H_

#include <glib.h>
#include "../elim-rpc.h"

xmlnode * _h_elim_response ( const char *name , 
                             const char *id   , 
                             SEXP_VALUE *args , 
                             gpointer    data );
#endif
