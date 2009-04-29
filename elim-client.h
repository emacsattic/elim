#ifndef _IM_CLIENT_H_
#define _IM_CLIENT_H_

#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <purple.h>

#include "elim-client-queue.h"
#include "elim-func-handlers.h"
#include "xnode/xnode.h"
#include "sexp/sexp-xml.h"
#include "sexp/sexp-util.h"

typedef enum _xmlrpc_type
{
    XRPC_UNKNOWN ,
    XRPC_INTEGER ,
    XRPC_BOOLEAN ,
    XRPC_FLOAT   ,
    XRPC_STRING  ,
    XRPC_BASE64  ,
    XRPC_ALIST   ,
    XRPC_LIST    ,
    XRPC_DATE    ,
    XRPC_NIL         
} xmlrpc_type;

#endif
