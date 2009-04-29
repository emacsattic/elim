#include "add_account.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

static void _h_elim_account_options ( PurpleAccount *acct, GHashTable *opts )
{
    fprintf( stderr, "oops, not handling extended account options yet\n" );
}

xmlnode * _h_elim_add_account ( const char *name , 
                                const char *id   ,
                                SEXP_VALUE *args , 
                                gpointer    data )
{
    ASSERT_ALISTP( args, id, name );

    char          *aname = ALIST_VAL_STRING( args, "account-name" );
    char          *proto = ALIST_VAL_STRING( args, "im-protocol"  );
    char          *pass  = ALIST_VAL_STRING( args, "password"     );
    GHashTable    *opts  = ALIST_VAL_ALIST ( args, "options"      );
    gboolean      is_new = FALSE;

    if( !aname || !*aname || !proto || !*proto )
    {
        sexp_val_free( args );
        return response_error( EINVAL, id, name, "account/protocol missing" );
    }

    if( !find_plugin_by_protocol(proto) )
    {
        sexp_val_free( args );
        return response_error( EINVAL, id, name, "invalid protocol" );
    }

    PurpleAccount *acct  = purple_accounts_find( aname, proto );
    const char    *ui    = purple_core_get_ui();
    if( !acct )
    {   
        acct   = purple_account_new( aname, proto );
        is_new = TRUE;
    }
    
    elim_ping();

    if( !acct )
    {
        sexp_val_free( args );
        return response_error( EINVAL, id, name, "account not initialised" );
    }

    // =================================================================
    if( is_new        ) purple_account_set_enabled ( acct , ui   , FALSE );
    if( pass && *pass ) purple_account_set_password( acct , pass );
    if( is_new        ) purple_accounts_add        ( acct );
    _h_elim_account_options( acct , opts );

    //purple_account_set_enabled ( acct , ui   , TRUE  );
    // =================================================================

    xmlnode *rval = xnode_new( "alist" );
    AL_INT( rval, "account-uid" , (int)acct );
    AL_STR( rval, "account-name", purple_account_get_username   ( acct ) );
    AL_STR( rval, "im-protocol" , purple_account_get_protocol_id( acct ) );

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
