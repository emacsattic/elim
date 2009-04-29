#include "default.h"

xmlnode * _h_elim_default( const char *name , 
                           const char *id   , 
                           SEXP_VALUE *args , 
                           gpointer data    )
{
    char *out = sexp_to_str( args );
    fprintf( stderr, ";; unknown function %s:\n%s\n;; ++ --\n", name, out );
    g_free ( out );
    return response_error( EINVAL, id, name, "unknown function" );
}
