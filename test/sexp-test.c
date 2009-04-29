#include <stdio.h>
#include <stdlib.h>

#include "../xnode/xnode.h"
#include "sexp-example.h"
#include "../sexp/sexp-xml.h"

int main ( int argc, char **argv )
{
    int      len = 0;
    xmlnode *xml = xnode_from_sexp( EXAMPLE_SEXP_METHODCALL );
    char    *rpc = xnode_to_formatted_str( xml, &len );
    char   *sexp = xnode_to_sexp         ( xml, &len );
    fprintf( stdout, "sexp in :\n%s\n" , EXAMPLE_SEXP_METHODCALL );
    fprintf( stdout, "xmlrpc  :\n%s\n" , rpc  );
    fprintf( stdout, "sexp out:\n%s\n" , sexp );
 
    g_free    ( rpc  );
    g_free    ( sexp );
    xnode_free( xml  );
}
