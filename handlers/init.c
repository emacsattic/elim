#include "init.h"
#include "../ui_ops/ops.h"

xmlnode * _h_elim_init ( const char *name ,
                         const char *id   ,
                         SEXP_VALUE *args ,
                         gpointer data    )
{
    ASSERT_ALISTP( args, id, name );

    char *dir = ALIST_VAL_STRING( args, "dot-dir" );
    char *ui  = ALIST_VAL_STRING( args, "ui-id"   );

    if( !ui ) { ui = "elim"; }

    purple_util_set_user_dir       ( dir  );
    purple_util_init               ();
    purple_core_set_ui_ops         ( &elim_core_ui_ops         );
    purple_eventloop_set_ui_ops    ( &elim_eventloop_ui_ops    );
    purple_blist_set_ui_ops        ( &elim_blist_ui_ops        );
    purple_accounts_set_ui_ops     ( &elim_account_ui_ops      );
    purple_request_set_ui_ops      ( &elim_request_ui_ops      );
    purple_idle_set_ui_ops         ( &elim_idle_ui_ops         );
    purple_connections_set_ui_ops  ( &elim_connections_ui_ops  );
    purple_conversations_set_ui_ops( &elim_conversation_ui_ops );

    // load any data for init:    
    if( purple_get_core() == NULL )
    {
        purple_core_init( ui );
        purple_set_blist( purple_blist_new() );
        purple_prefs_load();
        purple_blist_load();
    }
    else
    {
        const char *cur_ui = purple_core_get_ui();
        if( strcmp( cur_ui, name ) )
        {
            sexp_val_free( args );
            return response_error( EINVAL, id, name, 
                                   "purple has already been initialised" );
        }
    }

    sexp_val_free( args );
    xmlnode *rval = xnode_new( "alist" );
    AL_STR( rval, "ui-id", purple_core_get_ui() );
    return response_value( 0, id, name, rval );
}
