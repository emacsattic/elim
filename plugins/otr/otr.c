/*
 *  Off-the-Record Messaging plugin for libpurple
 *  Copyright © 2004-2008  Ian Goldberg, Rob Smits,
 *                         Chris Alexander, Nikita Borisov
 *                         <otr@cypherpunks.ca>
 *  Copyright © 2009       Vivek Dasmohapatra <vivek@etla.org>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of version 2 of the GNU General Public License as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "otr.h"
#include "otr-ui.h"
#include "otr-dialog.h"
#include "otr-ui-ops.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <gcrypt.h>

/* libpurple headers */
#include "notify.h"
#include "version.h"
#include "util.h"
#include "debug.h"
#include "core.h"

#include <glib/gi18n-lib.h>

#include <libotr/privkey.h>
#include <libotr/proto.h>
#include <libotr/tlv.h>
#include <libotr/message.h>
#include <libotr/userstate.h>

PurplePlugin *otrg_plugin_handle;

/* We'll only use the one OtrlUserState. */
OtrlUserState otrg_plugin_userstate = NULL;

/* GLib HashTable for storing the maximum message size for various
 * protocols. */
GHashTable* mms_table;

/* Send an IM from the given account to the given recipient.  Display an
 * error dialog if that account isn't currently logged in. */
void otrg_plugin_inject_message( PurpleAccount *account , 
                                 const char *recipient  ,
                                 const char *message    )
{
    PurpleConnection *connection;

    connection = purple_account_get_connection( account );
    if( !connection ) 
    {
        const char *proto = purple_account_get_protocol_id( account );
        const char *aname = purple_account_get_username   ( account );

        PurplePlugin *p = purple_find_prpl( proto );
        char *msg =
          g_strdup_printf( _("You are not connected to account %s (%s)."),
                           aname,
                           (p && p->info->name)? p->info->name: _("Unknown") );
        otrg_dialog_notify_error( aname, proto, recipient,
                                  _("Not connected"), msg, NULL );
        g_free( msg );
        return;
    }

    serv_send_im( connection, recipient, message, 0 );
}

static OtrlPolicy policy_cb( void *opdata, ConnContext *context )
{
    PurpleAccount *account;
    OtrlPolicy     policy = OTRL_POLICY_DEFAULT;
    OtrgUiPrefs    prefs;

    if( !context ) return policy;

    account = purple_accounts_find( context->accountname, context->protocol );

    if( !account ) return policy;

    otrg_ui_get_prefs( &prefs, account, context->username );

    return prefs.policy;
}

static const char *protocol_name_cb( void *opdata, const char *proto )
{
    PurplePlugin *p = purple_find_prpl( proto );
    return p ? p->info->name : NULL;
}

static void protocol_name_free_cb( void *opdata, const char *proto )
{
    /* Do nothing, since we didn't actually allocate any memory in
     * protocol_name_cb. */
}


void otrg_plugin_create_privkey( const char *aname, const char *proto )
{
    OtrgDialogWaitHandle waithandle;

#ifndef WIN32
    mode_t mask;
#endif  /* WIN32 */
    FILE *privf;

    gchar *keyfile = g_build_filename( purple_user_dir(), PRIVKEYFNAME, NULL );

    if( !keyfile ) 
    {
        fprintf( stderr, _("Out of memory building filenames!\n") );
        return;
    }

#ifndef WIN32
    mask = umask (0077);
#endif  /* WIN32 */
    privf = g_fopen( keyfile, "w+b" );
#ifndef WIN32

    umask( mask );
#endif  /* WIN32 */

    g_free( keyfile );
    if( !privf )
    {
        fprintf( stderr, _("Could not write private key file\n") );
        return;
    }

    waithandle = otrg_dialog_private_key_wait_start( aname, proto );

    /* Generate the key */
    otrl_privkey_generate_FILEp( otrg_plugin_userstate, privf, aname, proto );
    fclose( privf );
    otrg_ui_update_fingerprint();

    /* Mark the dialog as done. */
    otrg_dialog_private_key_wait_done( waithandle );
}

static void create_privkey_cb(void       *opdata,
                              const char *aname ,
                              const char *proto )
{
    otrg_plugin_create_privkey( aname, proto );
}

static int is_logged_in_cb( void       *opdata ,
                            const char *aname  ,
                            const char *proto  , 
                            const char *target )
{
    PurpleAccount *account;
    PurpleBuddy   *buddy;

    account = purple_accounts_find( aname, proto );
    if( !account ) return -1;

    buddy   = purple_find_buddy( account, target );
    if( !buddy   ) return -1;

    return PURPLE_BUDDY_IS_ONLINE( buddy );
}

static void inject_message_cb( void       *opdata , 
                               const char *aname  ,
                               const char *proto  ,
                               const char *target , 
                               const char *mesg   )
{
    PurpleAccount *account = purple_accounts_find( aname, proto );

    if( !account )
    {
        PurplePlugin *p = purple_find_prpl( proto );
        char *label = (p && p->info->name) ? p->info->name : _("Unknown");
        char *ual   = _("Unknown account");
        char *msg   = 
          g_strdup_printf( _("Unknown account %s (%s)." ), aname, label );

        otrg_dialog_notify_error( aname, proto, target, ual, msg, NULL );

        g_free( msg );
        return;
    }

    otrg_plugin_inject_message( account, target, mesg );
}

static void notify_cb( void       *opdata    ,
                       OtrlNotifyLevel level ,
                       const char *aname     , 
                       const char *proto     , 
                       const char *user      ,
                       const char *title     , 
                       const char *primary   , 
                       const char *extra     )
{
    PurpleNotifyMsgType pl = PURPLE_NOTIFY_MSG_ERROR;

    switch( level ) 
    {
      case OTRL_NOTIFY_ERROR:
	    pl = PURPLE_NOTIFY_MSG_ERROR;
	    break;
      case OTRL_NOTIFY_WARNING:
	    pl = PURPLE_NOTIFY_MSG_WARNING;
	    break;
      case OTRL_NOTIFY_INFO:
	    pl = PURPLE_NOTIFY_MSG_INFO;
	    break;
    }

    otrg_dialog_notify_message( pl, aname, proto, user, title, primary, extra );
}

static int display_otr_message_cb( void       *opdata, 
                                   const char *aname ,
                                   const char *proto , 
                                   const char *user  , 
                                   const char *msg   )
{
    return otrg_dialog_display_otr_message( aname, proto, user, msg );
}

static void update_context_list_cb( void *opdata )
{
    otrg_ui_update_keylist();
}

static void confirm_fingerprint_cb( void       *opdata,
                                    OtrlUserState us  ,
                                    const char *aname ,
                                    const char *proto ,
                                    const char *user  ,
                                    unsigned char fingerprint[20] )
{
    otrg_dialog_unknown_fingerprint( us, aname, proto, user, fingerprint );
}

static void write_fingerprints_cb( void *opdata )
{
    otrg_plugin_write_fingerprints();
    otrg_ui_update_keylist();
    otrg_dialog_resensitize_all();
}

static void gone_secure_cb( void *opdata, ConnContext *context )
{
    otrg_dialog_connected( context );
}

static void gone_insecure_cb( void *opdata, ConnContext *context )
{
    otrg_dialog_disconnected( context );
}

static void still_secure_cb( void *opdata, ConnContext *context, int is_reply )
{
    if( is_reply == 0 ) 
        otrg_dialog_stillconnected( context );
}

static void log_message_cb( void *opdata, const char *message )
{
    purple_debug_info( "otr", message );
}

static int max_message_size_cb( void *opdata, ConnContext *context )
{
    void* lookup_result = g_hash_table_lookup( mms_table, context->protocol );
    return lookup_result ? *( (int *)lookup_result ) : 0;
}

static OtrlMessageAppOps ui_ops =
{
    policy_cb              ,
    create_privkey_cb      ,
    is_logged_in_cb        ,
    inject_message_cb      ,
    notify_cb              ,
    display_otr_message_cb ,
    update_context_list_cb ,
    protocol_name_cb       ,
    protocol_name_free_cb  ,
    confirm_fingerprint_cb ,
    write_fingerprints_cb  ,
    gone_secure_cb         ,
    gone_insecure_cb       ,
    still_secure_cb        ,
    log_message_cb         ,
    max_message_size_cb    ,
    NULL                   , /* account_name */
    NULL                     /* account_name_free */
};

static void process_sending_im( PurpleAccount *account,
                                char *who      ,
                                char **message , 
                                void *m        )
{
    char        *nmesg = NULL;
    const char  *aname = purple_account_get_username   ( account );
    const char  *proto = purple_account_get_protocol_id( account );
    char        *user  = NULL;
    gcry_error_t err;

    if( !who || !message || !*message )
        return;

    user = g_strdup( purple_normalize( account, who ) );
    err  = 
      otrl_message_sending( otrg_plugin_userstate, 
                            &ui_ops , NULL , aname , proto, user , 
                            *message, NULL , &nmesg, NULL , NULL );

    if( err && !nmesg ) 
    {
        /* Be *sure* not to send out plaintext */
        char *ourm = strdup("");
        free( *message );
        *message = ourm;
    }
    else if( nmesg ) 
    {
        /* Fragment the message if necessary, and send all but the last
         * fragment over the network.  Pidgin will send the last
         * fragment for us. */
        ConnContext *context =
          otrl_context_find( otrg_plugin_userstate, user, aname, proto,
                             0, NULL, NULL, NULL );
        free( *message );
        *message = NULL;
        err = otrl_message_fragment_and_send( &ui_ops, NULL, context, nmesg  , 
                                              OTRL_FRAGMENT_SEND_ALL_BUT_LAST,
                                              message );
        otrl_message_free( nmesg );
    }
    free( user );
}

/* Abort the SMP protocol.  Used when malformed or unexpected messages
 * are received. */
void otrg_plugin_abort_smp( ConnContext *context )
{
    otrl_message_abort_smp( otrg_plugin_userstate, &ui_ops, NULL, context );
}

/* Start the Socialist Millionaires' Protocol over the current connection,
 * using the given initial secret, and optionally a question to pass to
 * the buddy. */
void otrg_plugin_start_smp( ConnContext  *context , 
                            const char   *question,
                            const guchar *secret  , 
                            size_t secretlen      )
{
    otrl_message_initiate_smp_q( otrg_plugin_userstate, &ui_ops, NULL,
                                 context, question, secret, secretlen );
}

/* Continue the Socialist Millionaires' Protocol over the current connection,
 * using the given initial secret (ie finish step 2). */
void otrg_plugin_continue_smp( ConnContext *context ,
                               const guchar *secret , 
                               size_t     secretlen )
{
    otrl_message_respond_smp( otrg_plugin_userstate, &ui_ops, NULL,
                              context, secret, secretlen );
}

/* Send the default OTR Query message to the correspondent of the given
 * context, from the given account.  [account is actually a
 * PurpleAccount*, but it's declared here as void* so this can be passed
 * as a callback.] */
void otrg_plugin_send_default_query( ConnContext *context, void *vaccount )
{
    PurpleAccount *account = vaccount;
    char *msg;
    OtrgUiPrefs prefs;

    otrg_ui_get_prefs( &prefs, account, context->username );
    msg = otrl_proto_default_query_msg( context->accountname, prefs.policy );
    otrg_plugin_inject_message( account, context->username, 
                                msg ? msg : "?OTRv2?" );
    free( msg );
}

/* Send the default OTR Query message to the correspondent of the given
 * conversation. */
void otrg_plugin_send_default_query_conv( PurpleConversation *conv )
{
    PurpleAccount *acct;
    const char *user, *aname;
    char *msg;
    OtrgUiPrefs prefs;
    
    acct  = purple_conversation_get_account( conv );
    user  = purple_conversation_get_name   ( conv );
    aname = purple_account_get_username    ( acct );
    
    otrg_ui_get_prefs( &prefs, acct, user );
    msg = otrl_proto_default_query_msg( aname, prefs.policy );
    otrg_plugin_inject_message( acct, user, msg ? msg : "?OTRv2?" );
    free( msg );
}

static gboolean process_receiving_im( PurpleAccount *account, 
                                      char **who     , 
                                      char **message , 
                                      int  *flags    , 
                                      void *m        )
{
    char        *nmsg    = NULL;
    OtrlTLV     *tlvs    = NULL;
    OtrlTLV     *tlv     = NULL;
    char        *user    = NULL;
    const char  *aname   = NULL;
    const char  *proto   = NULL;
    ConnContext *context = NULL;
    gboolean     res     = FALSE;
    NextExpectedSMP nextMsg;

    if( !who || !*who || !message || !*message )
        return 0;

    user  = g_strdup( purple_normalize    ( account , *who ) );
    aname = purple_account_get_username   ( account );
    proto = purple_account_get_protocol_id( account );

    res = otrl_message_receiving( otrg_plugin_userstate,
                                  &ui_ops , NULL , aname, proto, user ,
                                  *message, &nmsg, &tlvs, NULL , NULL );

    if( nmsg )
    {
        char *ourm = malloc(strlen(nmsg) + 1);
        if( ourm ) 
            strcpy( ourm, nmsg );
        otrl_message_free( nmsg );
        free( *message );
        *message = ourm;
    }

    
    if( (tlv = otrl_tlv_find( tlvs, OTRL_TLV_DISCONNECTED )) )
    {
        /* Notify the user that the other side disconnected. */
        otrg_dialog_finished( aname, proto, user );
        otrg_ui_update_keylist();
    }
    
    /* Keep track of our current progress in the Socialist Millionaires'
     * Protocol. */
    context = otrl_context_find( otrg_plugin_userstate, user,
                                 aname, proto, 0, NULL, NULL, NULL);
    if( context ) 
    {
        nextMsg = context->smstate->nextExpected;

        if( context->smstate->sm_prog_state == OTRL_SMP_PROG_CHEATED ) 
        {
            otrg_plugin_abort_smp ( context );
            otrg_dialog_update_smp( context , 0.0 );
            context->smstate->nextExpected  = OTRL_SMP_EXPECT1;
            context->smstate->sm_prog_state = OTRL_SMP_PROG_OK;
        } 
        else 
        {
            
            if( (tlv = otrl_tlv_find( tlvs, OTRL_TLV_SMP1Q )) )
            {
                if( nextMsg != OTRL_SMP_EXPECT1)
                    otrg_plugin_abort_smp(context);
                else 
                {
                    char *question = (char *)tlv->data;
                    char *eoq      = memchr( question, '\0', tlv->len );
                    if( eoq ) 
                        otrg_dialog_socialist_millionaires_q(context, question);
                }
            }
            
            if( (tlv = otrl_tlv_find( tlvs, OTRL_TLV_SMP1 )) ) 
            {
                if( nextMsg != OTRL_SMP_EXPECT1 )
                    otrg_plugin_abort_smp( context );
                else 
                    otrg_dialog_socialist_millionaires( context );
            }
            
            if( (tlv = otrl_tlv_find(tlvs, OTRL_TLV_SMP2)) ) 
            {
                if( nextMsg != OTRL_SMP_EXPECT2 )
                    otrg_plugin_abort_smp(context);
                else 
                {
                    otrg_dialog_update_smp( context, 0.6 );
                    context->smstate->nextExpected = OTRL_SMP_EXPECT4;
                }
            }
            
            if( (tlv = otrl_tlv_find(tlvs, OTRL_TLV_SMP3)) ) 
            {
                if( nextMsg != OTRL_SMP_EXPECT3 )
                    otrg_plugin_abort_smp( context );
                else 
                {
                    otrg_dialog_update_smp( context, 1.0 );
                    context->smstate->nextExpected = OTRL_SMP_EXPECT1;
                }
            }
            
            if( (tlv = otrl_tlv_find(tlvs, OTRL_TLV_SMP4)) ) 
            {
                if( nextMsg != OTRL_SMP_EXPECT4 )
                    otrg_plugin_abort_smp( context );
                else 
                {
                    otrg_dialog_update_smp( context, 1.0 );
                    context->smstate->nextExpected = OTRL_SMP_EXPECT1;
                }
            }

            tlv = otrl_tlv_find( tlvs, OTRL_TLV_SMP_ABORT );
            if( tlv ) 
            {
                otrg_dialog_update_smp( context, 0.0 );
                context->smstate->nextExpected = OTRL_SMP_EXPECT1;
            }
        }
    }

    otrl_tlv_free( tlvs );
    free( user );

    /* If we're supposed to ignore this incoming message (because it's a
     * protocol message), set it to NULL, so that other plugins that
     * catch receiving-im-msg don't return 0, and cause it to be
     * displayed anyway. */
    if( res ) 
    {
        free( *message );
        *message = NULL;
    }

    return res;
}

static void process_conv_create( PurpleConversation *conv, void *data )
{
    if( conv ) otrg_dialog_new_conv( conv );
}

static void process_conv_updated( PurpleConversation  *conv ,
                                  PurpleConvUpdateType type ,
                                  void *data                )
{
    /* See if someone's trying to turn logging on for this conversation,
     * and we don't want them to. */
    if( type == PURPLE_CONV_UPDATE_LOGGING ) 
    {
        ConnContext   *context;
        OtrgUiPrefs    prefs;
        PurpleAccount *account = purple_conversation_get_account( conv );
        const char    *cname   = purple_conversation_get_name   ( conv );

        otrg_ui_get_prefs( &prefs, account, cname );

        context = otrg_plugin_conv_to_context( conv );

        if( context                                      && 
            prefs.avoid_logging_otr                      &&
            context->msgstate == OTRL_MSGSTATE_ENCRYPTED &&
            conv->logging == TRUE                        )
        { 
            purple_conversation_set_logging( conv, FALSE );
        }
    }
}

static void process_connection_change( PurpleConnection *conn, void *data )
{
    /* If we log in or out of a connection, make sure all of the OTR
     * buttons are in the appropriate sensitive/insensitive state. */
    otrg_dialog_resensitize_all();
}

static void otr_options_cb( PurpleBlistNode *node, gpointer user_data )
{
    /* We've already checked PURPLE_BLIST_NODE_IS_BUDDY(node) */
    PurpleBuddy *buddy = (PurpleBuddy *)node;

    /* Modify the settings for this buddy */
    otrg_ui_config_buddy( buddy );
}

static void supply_extended_menu( PurpleBlistNode *node, GList **menu )
{
    PurpleMenuAction *act;
    PurpleBuddy      *buddy;
    PurpleAccount    *acct;
    const char       *proto;

    if( !PURPLE_BLIST_NODE_IS_BUDDY( node ) ) return;

    /* Extract the account, and then the protocol, for this buddy */
    buddy = (PurpleBuddy *)node;
    acct = buddy->account;

    if( acct == NULL ) return;

    proto = purple_account_get_protocol_id( acct );

    if( !otrg_plugin_proto_supports_otr( proto ) ) return;

    act = purple_menu_action_new( _("OTR Settings"),
                                  (PurpleCallback)otr_options_cb, NULL, NULL );
    *menu = g_list_append( *menu, act );
}

/* Disconnect a context, sending a notice to the other side, if appropriate. */
void otrg_plugin_disconnect( ConnContext *context )
{
    otrl_message_disconnect( otrg_plugin_userstate, 
                             &ui_ops, NULL,
                             context->accountname , 
                             context->protocol    , 
                             context->username    );
}

/* Write the fingerprints to disk. */
void otrg_plugin_write_fingerprints (void)
{
#ifndef WIN32
    mode_t mask;
#endif  /* WIN32 */
    FILE  *storef;
    gchar *storefile = g_build_filename(purple_user_dir(), STOREFNAME, NULL);
#ifndef WIN32
    mask = umask( 0077 );
#endif  /* WIN32 */
    storef = g_fopen( storefile, "wb" );
#ifndef WIN32
    umask( mask );
#endif  /* WIN32 */
    g_free( storefile );

    if( !storef ) return;

    otrl_privkey_write_fingerprints_FILEp( otrg_plugin_userstate, storef );
    fclose( storef );
}

/* Find the ConnContext appropriate to a given PurpleConversation. */
ConnContext *otrg_plugin_conv_to_context( PurpleConversation *conv )
{
    PurpleAccount *acct;
    char          *user;
    const char    *cname;
    const char    *aname;
    const char    *proto;
    ConnContext   *context;

    acct  = purple_conversation_get_account( conv );
    cname = purple_conversation_get_name   ( conv );
    aname = purple_account_get_username    ( acct );
    proto = purple_account_get_protocol_id ( acct );
    user  = g_strdup( purple_normalize( acct, cname ) );

    context = otrl_context_find( otrg_plugin_userstate, 
                                 user, aname, proto, 0, NULL, NULL, NULL );
    g_free( user );

    return context;
}

/* Find the PurpleConversation appropriate to the given userinfo.  If
 * one doesn't yet exist, create it if force_create is true. */
PurpleConversation *otrg_plugin_userinfo_to_conv( const char *aname,
                                                  const char *proto, 
                                                  const char *user , 
                                                  int create       )
{
    PurpleAccount      *acct;
    PurpleConversation *conv;

    if( !(acct = purple_accounts_find( aname, proto ) ) ) return NULL;

    conv =
      purple_find_conversation_with_account( PURPLE_CONV_TYPE_IM, user, acct );

    if( !conv && create )
        conv = purple_conversation_new( PURPLE_CONV_TYPE_IM, acct, user );

    return conv;
}

/* Find the PurpleConversation appropriate to the given ConnContext.  If
 * one doesn't yet exist, create it if force_create is true. */
PurpleConversation *otrg_plugin_context_to_conv( ConnContext *context,
                                                 int create)
{
    return otrg_plugin_userinfo_to_conv( context->accountname,
                                         context->protocol   ,
                                         context->username   , create );
}

/* What level of trust do we have in the privacy of this ConnContext? */
TrustLevel otrg_plugin_context_to_trust( ConnContext *context )
{
    TrustLevel level = TRUST_NOT_PRIVATE;

    if( context && context->msgstate == OTRL_MSGSTATE_ENCRYPTED )
        if( context->active_fingerprint->trust &&
            context->active_fingerprint->trust[0] != '\0' ) 
            level = TRUST_PRIVATE;
        else 
            level = TRUST_UNVERIFIED;
    else if( context && context->msgstate == OTRL_MSGSTATE_FINISHED )
        level = TRUST_FINISHED;

    return level;
}

/* Send the OTRL_TLV_DISCONNECTED packets when we're about to quit. */
static void process_quitting (void)
{
    ConnContext *context = otrg_plugin_userstate->context_root;

    while( context )
    {
        ConnContext *next = context->next;
        if( context->msgstate == OTRL_MSGSTATE_ENCRYPTED &&
            context->protocol_version > 1 ) 
            otrg_plugin_disconnect( context );
        context = next;
    }
}

/* Read the maxmsgsizes from a FILE* into the given GHashTable.
 * The FILE* must be open for reading. */
static void mms_read_FILEp ( FILE *mmsf, GHashTable *ght )
{
    char storeline[50];
    size_t maxsize = sizeof(storeline);

    if( !mmsf ) return;

    while( fgets( storeline, maxsize, mmsf ) )
    {
        char *proto;
        char *prot_in_table;
        char *mms;
        int  *mms_in_table;
        char *tab;
        char *eol;
        /* Parse the line, which should be of the form:
         *    protocol\tmaxmsgsize\n          */
        proto = storeline;
        tab   = strchr( proto, '\t' );

        if( !tab ) continue;
        *tab = '\0';
        mms  = tab + 1;
        tab  = strchr( mms, '\t' );

        if( tab ) continue;
        eol = strchr(mms, '\r');

        if( !eol ) eol = strchr( mms, '\n' );
        if( !eol ) continue;
        *eol = '\0';
	
        prot_in_table = strdup( proto );
        mms_in_table  = malloc( sizeof(int) );
        *mms_in_table = atoi  ( mms );

        g_hash_table_insert( ght, prot_in_table, mms_in_table );
    }
}

static void otrg_str_free( gpointer data )
{
    g_free( (char*)data );
}

static void otrg_int_free( gpointer data )
{
    g_free( (int*)data );
}

static void otrg_init_mms_table ()
{
    /* Hardcoded defaults for maximum message sizes for various
     * protocols.  These can be overridden in the user's MAXMSGSIZEFNAME
     * file. */
    static const struct s_OtrgIdProtPair 
    {
        char *protid;
        int maxmsgsize;
    } mmsPairs[8] = { { "prpl-msn"  , 1409 } ,
                      { "prpl-icq"  , 2346 } ,
                      { "prpl-aim"  , 2343 } ,
                      { "prpl-yahoo", 832  } , 
                      { "prpl-gg"   , 1999 } ,
                      { "prpl-irc"  , 417  } ,
                      { "prpl-oscar", 2343 } , 
                      { NULL        , 0    } };
    int i = 0;
    gchar *maxmsgsizefile;
    FILE  *mmsf;

    mms_table = g_hash_table_new_full( g_str_hash    ,
                                       g_str_equal   ,
                                       otrg_str_free , 
                                       otrg_int_free );

    for( i=0; mmsPairs[i].protid != NULL; i++ )
    {
    	char* nextprot = g_strdup( mmsPairs[i].protid );
    	int*  nextsize = g_malloc( sizeof(int) );
    	*nextsize = mmsPairs[i].maxmsgsize;
    	g_hash_table_insert( mms_table, nextprot, nextsize );
    }

    maxmsgsizefile = g_build_filename( purple_user_dir(),
                                       MAXMSGSIZEFNAME, NULL );

    if( maxmsgsizefile )
    {
        mmsf = g_fopen( maxmsgsizefile, "rt" );
        /* Actually read the file here */
        if( mmsf )
        {
            mms_read_FILEp( mmsf, mms_table );
            fclose( mmsf );
        }
        g_free( maxmsgsizefile );
    }
}

static void otrg_free_mms_table()
{
    g_hash_table_destroy( mms_table );
}

static gboolean otr_plugin_load( PurplePlugin *handle )
{
    gchar *store   = g_build_filename( purple_user_dir(), STOREFNAME, NULL );
    void  *conv_h  = purple_conversations_get_handle();
    void  *conn_h  = purple_connections_get_handle();
    void  *blist_h = purple_blist_get_handle();
    void  *core_h  = purple_get_core();
    gchar *privkey = g_build_filename( purple_user_dir(), PRIVKEYFNAME, NULL );
    FILE  *privf;
    FILE  *storef;

    if( !privkey || !store) 
    {
        g_free( privkey );
        g_free( store   );
        return FALSE;
    }

    privf  = g_fopen( privkey, "rb" );
    storef = g_fopen( store  , "rb" );
    g_free( privkey );
    g_free( store   );

    otrg_init_mms_table();

    otrg_plugin_handle = handle;

    /* Make our OtrlUserState; we'll only use the one. */
    otrg_plugin_userstate = otrl_userstate_create();

    otrl_privkey_read_FILEp( otrg_plugin_userstate, privf );
    otrl_privkey_read_fingerprints_FILEp( otrg_plugin_userstate, storef,
                                          NULL, NULL);
    if ( privf  ) fclose( privf  );
    if ( storef ) fclose( storef );

    otrg_ui_update_fingerprint();

    purple_debug_info( "OTR", "signals-connect" );
    purple_signal_connect( core_h, "quitting",
                           otrg_plugin_handle,
                           PURPLE_CALLBACK(process_quitting), NULL );

    purple_signal_connect( conv_h, "sending-im-msg",
                           otrg_plugin_handle,
                           PURPLE_CALLBACK(process_sending_im), NULL );

    purple_signal_connect( conv_h, "receiving-im-msg",
                           otrg_plugin_handle,
                           PURPLE_CALLBACK(process_receiving_im), NULL );

    purple_signal_connect( conv_h, "conversation-updated",
                           otrg_plugin_handle,
                           PURPLE_CALLBACK(process_conv_updated), NULL);
    
    purple_signal_connect( conv_h, "conversation-created",
                           otrg_plugin_handle, 
                           PURPLE_CALLBACK(process_conv_create), NULL);
    
    purple_signal_connect( conn_h, "signed-on",
                           otrg_plugin_handle,
                           PURPLE_CALLBACK(process_connection_change), NULL);

    purple_signal_connect( conn_h, "signed-off",
                           otrg_plugin_handle,
                           PURPLE_CALLBACK(process_connection_change), NULL);

    purple_signal_connect( blist_h, "blist-node-extended-menu",
                           otrg_plugin_handle, 
                           PURPLE_CALLBACK(supply_extended_menu), NULL );
    purple_debug_info( "OTR", "signals-connected" );
    otrg_ui_init();
    otrg_dialog_init();

    purple_conversation_foreach( otrg_dialog_new_conv );

    return 1;
}

static gboolean otr_plugin_unload( PurplePlugin *handle )
{
    void *conv_h  = purple_conversations_get_handle();
    void *conn_h  = purple_connections_get_handle();
    void *blist_h = purple_blist_get_handle();
    void *core_h  = purple_get_core();

    /* Clean up all of our state. */
    otrl_userstate_free( otrg_plugin_userstate );
    otrg_plugin_userstate = NULL;

    otrg_free_mms_table();

    purple_signal_disconnect( core_h, "quitting", 
                              otrg_plugin_handle,
                              PURPLE_CALLBACK(process_quitting) );

    purple_signal_disconnect( conv_h, "sending-im-msg",
                              otrg_plugin_handle,
                              PURPLE_CALLBACK(process_sending_im) );

    purple_signal_disconnect( conv_h, "receiving-im-msg",
                              otrg_plugin_handle,
                              PURPLE_CALLBACK(process_receiving_im) );

    purple_signal_disconnect( conv_h, "conversation-updated",
                              otrg_plugin_handle,
                              PURPLE_CALLBACK(process_conv_updated) );

    purple_signal_disconnect( conv_h, "conversation-created",
                              otrg_plugin_handle, 
                              PURPLE_CALLBACK(process_conv_create) );

    purple_signal_disconnect( conn_h, "signed-on", 
                              otrg_plugin_handle,
                              PURPLE_CALLBACK(process_connection_change) );

    purple_signal_disconnect( conn_h, "signed-off",
                              otrg_plugin_handle,
                              PURPLE_CALLBACK(process_connection_change) );

    purple_signal_disconnect( blist_h, "blist-node-extended-menu",
                              otrg_plugin_handle,
                              PURPLE_CALLBACK(supply_extended_menu) );



    purple_conversation_foreach( otrg_dialog_remove_conv );

    otrg_dialog_cleanup();
    otrg_ui_cleanup();

    return 1;
}

/* Return 1 if the given protocol supports OTR, 0 otherwise. */
int otrg_plugin_proto_supports_otr( const char *proto )
{
    /* Right now, OTR should work on all protocols, possibly
     * with the help of fragmentation. */
    return 1;
}

#define UI_INFO NULL
#define PLUGIN_TYPE NULL

static PurplePluginInfo info =
{
	PURPLE_PLUGIN_MAGIC,

    /* Use the 2.0.x API */
    2,                                                /* major version  */
	0,                                                /* minor version  */

	PURPLE_PLUGIN_STANDARD,                           /* type           */
	PLUGIN_TYPE,                                      /* ui_requirement */
	0,                                                /* flags          */
	NULL,                                             /* dependencies   */
	PURPLE_PRIORITY_DEFAULT,                          /* priority       */
	"otr",                                            /* id             */
	NULL,                                             /* name           */
	OTRL_VERSION,                                     /* version        */
	NULL,                                             /* summary        */
	NULL,                                             /* description    */
	                                                  /* author         */
	"Ian Goldberg, Rob Smits,\n"
    "\t\t\tChris Alexander, Nikita Borisov\n"
    "\t\t\t<otr@cypherpunks.ca>",
	"http://otr.cypherpunks.ca/",                     /* homepage       */

	otr_plugin_load,                                  /* load           */
	otr_plugin_unload,                                /* unload         */
	NULL,                                             /* destroy        */

	UI_INFO,                                          /* ui_info        */
	NULL,                                             /* extra_info     */
	NULL,                                             /* prefs_info     */
	NULL                                              /* actions        */
};

static void
__init_plugin( PurplePlugin *plugin )
{
    /* Set up the UI ops */
    otrg_ui_set_ui_ops    ( otrui_get_ui_ops       () );
  //otrg_dialog_set_ui_ops( otrui_dialog_get_ui_ops() );

#ifndef WIN32
    /* Make key generation use /dev/urandom instead of /dev/random */
    gcry_control( GCRYCTL_ENABLE_QUICK_RANDOM, 0 );
#endif

    /* Initialize the OTR library */
    OTRL_INIT;

#ifdef ENABLE_NLS
    bindtextdomain( GETTEXT_PACKAGE, LOCALEDIR );
    bind_textdomain_codeset( GETTEXT_PACKAGE, "UTF-8" );
#endif

    info.name        = _("Off-the-Record Messaging");
    info.summary     = _("Provides private and secure conversations");
    info.description = _("Preserves the privacy of IM communications "
                         "by providing encryption, authentication, "
                         "deniability, and perfect forward secrecy.");

}

PURPLE_INIT_PLUGIN( otr, __init_plugin, info );
