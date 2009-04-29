#include "list_protocols.h"
#include "../ui_ops/ops.h"

xmlnode * _h_elim_list_protocols ( const char *name ,
                                   const char *id   ,
                                   SEXP_VALUE *args ,
                                   gpointer data    )
{
    xmlnode *rval = xnode_new( "alist" );
    GList  *plist = NULL;

    elim_ping();

    for( plist = purple_plugins_get_protocols(); plist; plist = plist->next )
    {
        PurplePlugin *plugin = plist->data;
        if( !plugin                              ) continue;
        if( !PURPLE_IS_PROTOCOL_PLUGIN( plugin ) ) continue;

        const char *key = purple_plugin_get_id  ( plugin );
        const char *val = purple_plugin_get_name( plugin );

        AL_STR( rval, key, val );
    }

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
