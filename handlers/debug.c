#include "debug.h"
#include "../ui_ops/ops.h"

xmlnode * _h_elim_debug ( const char *name ,
                          const char *id   ,
                          SEXP_VALUE *args ,
                          gpointer data    )
{
    gboolean debug;
    
    if( args )
    {
        debug = 
          ( ( args->type == SEXP_ALIST ) ? ALIST_VAL_BOOL( args, "debug" ) :
            ( args->type == SEXP_BOOL  ) ? args->x.bool                    : 
            !purple_debug_is_enabled() );
    }
    else 
        debug = !purple_debug_is_enabled();

    purple_debug_set_enabled( debug );
    debug = purple_debug_is_enabled();

    sexp_val_free( args );
    xmlnode *rval = xnode_new( "bool" );
    xnode_insert_data( rval, debug ? "1" : "0", 1 );
    return response_value( 0, id, name, rval );
}
