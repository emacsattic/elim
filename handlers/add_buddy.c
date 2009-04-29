#include "add_buddy.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"


xmlnode * _h_elim_add_buddy ( const char *name ,
                              const char *id   ,
                              SEXP_VALUE *args ,
                              gpointer data    )
{
    ASSERT_ALISTP( args, id, name );

    elim_ping();
    
    const char *aname = ALIST_VAL_STR( args, "account-name" );
    const char *proto = ALIST_VAL_STR( args, "im-protocol"  );
    gpointer    auid  = ALIST_VAL_PTR( args, "account-uid"  );

    PurpleAccount *acct = 
      auid ? find_acct_by_uid( auid ) : purple_accounts_find( aname, proto );

    if( !acct )
    {
        sexp_val_free( args );
        return response_error( ENXIO, id, name, "unknown account" );
    }

    const char *b_arg = ALIST_VAL_STRING( args, "buddy-name" );
    const char *bname = purple_normalize( acct, b_arg        );
    const char *gname = ALIST_VAL_STRING( args, "group"      );
    if( !gname || !*gname ) gname = "Buddies";

    PurpleGroup *group = purple_group_new( gname );
    PurpleBuddy *buddy = purple_buddy_new( acct, bname, b_arg );
    
    purple_blist_add_buddy  ( buddy, NULL, group, NULL );
    purple_account_add_buddy( acct , buddy );

    xmlnode *rval = xnode_new( "alist" );
    AL_INT( rval, "account-uid" , (int)acct  );
    AL_INT( rval, "buddy-uid"   , (int)buddy );
    AL_INT( rval, "group-uid"   , (int)group );
    AL_STR( rval, "buddy-name"  , purple_buddy_get_name ( buddy ) );
    AL_STR( rval, "account-name", purple_account_get_username   ( acct ) );
    AL_STR( rval, "im-protocol" , purple_account_get_protocol_id( acct ) );
    AL_STR( rval, "buddy-alias" , purple_buddy_get_alias( buddy ) );
    AL_STR( rval, "group-name"  , purple_group_get_name ( group ) );

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
