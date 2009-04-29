#ifndef _ELIM_REQUEST_UI_OPS_H_
#define _ELIM_REQUEST_UI_OPS_H_

#include <purple.h>
#include "../elim-rpc.h"
#include "../elim-client-queue.h"

extern PurpleRequestUiOps elim_request_ui_ops;

#define VA_LIST_TO_ALIST( alist, a_type, c_ktype, c_vtype, va ) \
    if( va )                                                    \
    {                                                           \
        c_ktype key = NULL;                                     \
        while( (key = va_arg( va, c_ktype )) )                  \
        {                                                       \
            c_vtype val = va_arg( va, c_vtype );                \
            AL_ ## a_type( alist, key, val );                   \
        }                                                       \
    }

#endif
