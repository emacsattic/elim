#include "connect.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"


xmlnode * _h_elim_connect ( const char *name , 
                            const char *id   ,
                            SEXP_VALUE *args , 
                            gpointer    data )
{
    ASSERT_ALISTP( args, id, name );

    elim_ping();
    
    const char *aname = ALIST_VAL_STR( args, "account-name" );
    const char *proto = ALIST_VAL_STR( args, "im-protocol"  );
    const char *ui    = purple_core_get_ui();
    gpointer    auid  = ALIST_VAL_PTR( args, "account-uid"  );

    PurpleAccount *acct = 
      auid ? find_acct_by_uid( auid ) : purple_accounts_find( aname, proto );

    if( !acct )
    {
        sexp_val_free( args );
        return response_error( ENXIO, id, name, "unknown account" );
    }

    // =================================================================
    purple_account_set_enabled ( acct , ui , TRUE );
    // =================================================================

    xmlnode *rval = xnode_new( "alist" );
    AL_INT( rval, "account-uid" , (int)acct );
    AL_STR( rval, "account-name", purple_account_get_username   ( acct ) );
    AL_STR( rval, "im-protocol" , purple_account_get_protocol_id( acct ) );

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
