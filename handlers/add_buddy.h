#ifndef _EMACSIM_HANDLER_ADD_BUDDY_H_
#define _EMACSIM_HANDLER_ADD_BUDDY_H_

#include "../elim-rpc.h"

xmlnode * _h_elim_add_buddy ( const char *name ,
                              const char *id   ,
                              SEXP_VALUE *args ,
                              gpointer data    );

#endif
