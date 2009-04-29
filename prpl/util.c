/*
Copyright © 2009 Vivek Dasmohapatra 

email : vivek@etla.org
irc   : fledermaus on freenode, oftc
jabber: fledermaus@jabber.earth.li

This file is part of elim.

elim is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

elim is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with elim.  If not, see <http://www.gnu.org/licenses/>.
*/
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
