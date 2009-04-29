#include "message.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

xmlnode * _h_elim_message ( const char *name ,
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

    PurpleConversationType pt = PURPLE_CONV_TYPE_IM;
    gpointer             cuid = ALIST_VAL_PTR( args, "conv-uid"  );
    const char         *cname = ALIST_VAL_STR( args, "conv-name" );
    PurpleConversation    *pc = find_conv_by_acct_uid( acct, cuid );

    if  ( pc ) pt = purple_conversation_get_type( pc );
    else       pc = purple_conversation_new     ( pt, acct, cname );

    if( !pc )
    {
        sexp_val_free( args );
        return response_error( ENXIO, id, name, "new conversation failed" );
    }

    PurpleConvIm   *pci = NULL;
    PurpleConvChat *pcc = NULL;
    const char     *msg = ALIST_VAL_STRING( args, "text" );
    char           *esc = g_markup_escape_text( msg, -1 );
    int             len = strlen( esc );

    switch( pt )
    {
      case PURPLE_CONV_TYPE_IM:
        pci = purple_conversation_get_im_data( pc );
        purple_conv_im_send( pci, esc );
        break;
      case PURPLE_CONV_TYPE_CHAT:
        pcc = purple_conversation_get_chat_data( pc );
        purple_conv_chat_send( pcc, esc );
        break;
      default:
        g_free       ( esc  );
        sexp_val_free( args );
        return response_error( EINVAL, id, name, "unknown conversation type" );
    }

    xmlnode *rval = xnode_new( "alist" );
    AL_INT( rval, "bytes", len );

    g_free       ( esc  );
    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
