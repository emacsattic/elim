#include "response.h"
#include "../elim-rpc.h"

xmlnode * _h_elim_response ( const char *name , 
                             const char *id   , 
                             SEXP_VALUE *args , 
                             gpointer    data )
{
    // this action frees the key in the hash, but not the value:
    // the callback handler function we extract here is in charge of freeing
    // the data pointer if this is required;
    // we are in charge of freeing the callback data pointer cbh:
    // the callback handler is also in charge of freeing the SEXP args:
    CB_HANDLER *cbh = fetch_cb_data( id );
    if( cbh )
    {
        CB_FUNC  func   = cbh->func;
        gpointer handle = cbh->data;
        xmlnode *rval   = NULL;
        if( func )
            rval = func( handle, args );
        g_free( cbh );
        return rval;
    }

    return NULL;
}
