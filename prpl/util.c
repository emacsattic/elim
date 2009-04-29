#include <glib.h>
#include <purple.h>
#include <string.h>

PurpleConversation *find_conv_by_acct_uid( PurpleAccount *acct, gpointer id )
{
    GList              *clist = NULL;
    PurpleConversation *conv  = NULL;

    for( clist = purple_get_conversations(); clist; clist = clist->next )
        if( id == clist->data ) { conv = clist->data; break; }

    return 
      ( conv ? (purple_conversation_get_account(conv) == acct) : FALSE ) ? 
        conv : NULL;
}

PurpleAccount *find_acct_by_uid(gpointer uid)
{
    GList         *alist = NULL;
    PurpleAccount *acct  = NULL;

    for( alist = purple_accounts_get_all(); alist; alist = alist->next )
        if( uid == alist->data ) { acct = alist->data; break; }

    return acct;
}

PurplePlugin *find_plugin_by_protocol( const char *name )
{
    GList        *plist = NULL;
    PurplePlugin *rval  = NULL;

    if( !name || !*name ) return NULL;

    for( plist = purple_plugins_get_protocols(); plist; plist = plist->next )
    {
        PurplePlugin *plugin = plist->data;
        if( !plugin                              ) continue;
        if( !PURPLE_IS_PROTOCOL_PLUGIN( plugin ) ) continue;
        const char *key = purple_plugin_get_id( plugin );
        if( !key || !*key || strcmp( name, key ) ) continue;
        rval = plugin;
        break;
    }

    return rval;
}
