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
#include "notify_ui_ops.h"

void *_elim_notify_message    ( PurpleNotifyMsgType type           ,
                                const char *title                  ,
                                const char *primary                ,
                                const char *secondary              );

void *_elim_notify_email      ( PurpleConnection *gc               ,
                                const char *subject                ,
                                const char *from                   ,
                                const char *to                     ,
                                const char *url                    );

void *_elim_notify_emails     ( PurpleConnection *gc               ,
                                size_t count                       ,
                                gboolean detailed                  ,
                                const char **subjects              ,
                                const char **froms                 ,
                                const char **tos                   ,
                                const char **urls                  );

void *_elim_notify_formatted  ( const char *title                  ,
                                const char *primary                ,
                                const char *secondary              ,
                                const char *text                   );

void *_elim_notify_search     ( PurpleConnection *gc               ,
                                const char *title                  ,
                                const char *primary                ,
                                const char *secondary              ,
                                PurpleNotifySearchResults *results ,
                                gpointer user_data                 );

void _elim_notify_search_rows ( PurpleConnection *gc               ,
                                PurpleNotifySearchResults *results ,
                                void *data                         );

void *_elim_notify_userinfo   ( PurpleConnection *gc               ,
                                const char *who                    ,
                                PurpleNotifyUserInfo *user_info    );

void *_elim_notify_uri        ( const char *uri                    );

void _elim_close_notify       ( PurpleNotifyType type              ,
                                void *ui_handle);

PurpleNotifyUiOps elim_notify_ui_ops = 
{
    _elim_notify_message     ,
    _elim_notify_email       ,
    _elim_notify_emails      ,
    _elim_notify_formatted   ,
    _elim_notify_search      ,
    _elim_notify_search_rows ,
    _elim_notify_userinfo    ,
    _elim_notify_uri         ,
    _elim_close_notify       ,
    NULL ,
    NULL ,
    NULL ,
    NULL
};

typedef struct _NOTIFY_RESP 
{
    char *id;                           // elim call id
    PurpleNotifyType    notify_type ;
} NOTIFY_RESP;


#define NOTIFY_START_FUNC \
    CB_HANDLER  *handle = g_new0( CB_HANDLER , 1 );  \
    NOTIFY_RESP *resp   = g_new0( NOTIFY_RESP, 1 );  \
    xmlnode     *alist  = xnode_new( "alist" );      \
    char        *ID     = new_elim_id()

#define NOTIFY_CLOSE_FUNC(NTYPE,NAME) \
    resp  ->id           = ID;                                       \
    resp  ->notify_type  = PURPLE_NOTIFY_ ## NTYPE;                  \
    handle->func         = _elim_notify_ ## NAME ## _cb;             \
    handle->data         = resp;                                     \
    store_cb_data( ID, handle );                                     \
    xmlnode *mcall = func_call( "elim-notify-" # NAME, ID, alist );  \
    add_outbound_sexp( mcall );                                      \
    return handle;


// *************************************************************************
static xmlnode  *_elim_notify_message_cb    ( gpointer data, SEXP_VALUE *args ) 
{
    CB_HANDLER *handle = data;
    purple_notify_close( PURPLE_NOTIFY_MESSAGE, handle );
    return NULL;
}
static xmlnode  *_elim_notify_email_cb      ( gpointer data, SEXP_VALUE *args )
{
    CB_HANDLER *handle = data;
    purple_notify_close( PURPLE_NOTIFY_EMAIL, handle );
    return NULL;
}
static xmlnode  *_elim_notify_emails_cb     ( gpointer data, SEXP_VALUE *args )
{
    CB_HANDLER *handle = data;
    purple_notify_close( PURPLE_NOTIFY_EMAILS, handle );
    return NULL;
}
static xmlnode  *_elim_notify_formatted_cb  ( gpointer data, SEXP_VALUE *args )
{
    CB_HANDLER *handle = data;
    fprintf( stderr, "_elim_notify_formatted_cb(%p, %p)\n", data, args );
    purple_notify_close( PURPLE_NOTIFY_FORMATTED, handle );
    if( args ) sexp_val_free( args );
    return NULL;
}
static xmlnode  *_elim_notify_search_cb     ( gpointer data, SEXP_VALUE *args )
{
    CB_HANDLER *handle = data;
    purple_notify_close( PURPLE_NOTIFY_SEARCHRESULTS, handle );
    return NULL;
}
static xmlnode  *_elim_notify_userinfo_cb   ( gpointer data, SEXP_VALUE *args )
{
    CB_HANDLER *handle = data;
    purple_notify_close( PURPLE_NOTIFY_USERINFO, handle );
    return NULL;
}
static xmlnode  *_elim_notify_uri_cb        ( gpointer data, SEXP_VALUE *args )
{
    CB_HANDLER *handle = data;
    purple_notify_close( PURPLE_NOTIFY_URI, handle );
    return NULL;
}

// *************************************************************************

void *_elim_notify_message ( PurpleNotifyMsgType type ,
                             const char *title        ,
                             const char *primary      ,
                             const char *secondary    )
{
    NOTIFY_START_FUNC;

    AL_STR ( alist, "title"       , title     );
    AL_STR ( alist, "primary"     , primary   );
    AL_STR ( alist, "secondary"   , secondary );
    AL_ENUM( alist, "message-type", type      , ":notify-msg-type" );

    NOTIFY_CLOSE_FUNC( MESSAGE, message );
}

void *_elim_notify_email ( PurpleConnection *gc ,
                           const char *subject  ,
                           const char *from     ,
                           const char *to       ,
                           const char *url      )
{
    NOTIFY_START_FUNC;

    PurpleAccount *acct  = gc   ? purple_connection_get_account ( gc   ) : NULL;
    const char    *aname = acct ? purple_account_get_username   ( acct ) : NULL;
    const char    *proto = acct ? purple_account_get_protocol_id( acct ) : NULL;

    AL_PTR ( alist, "account-uid" , acct    );
    AL_STR ( alist, "account-name", aname   );
    AL_STR ( alist, "im-protocol" , proto   );
    AL_STR ( alist, "subject"     , subject );
    AL_STR ( alist, "from"        , from    );
    AL_STR ( alist, "to"          , to      );
    AL_STR ( alist, "url"         , url     );

    NOTIFY_CLOSE_FUNC( EMAIL, email );
}

#define MAYBE_ATTRIBUTE(slot,alist) \
    if( slot ## s ) { AL_STR( alist, #slot, *slot ## s ); slot ## s++; }

void *_elim_notify_emails     ( PurpleConnection *gc               ,
                                size_t count                       ,
                                gboolean detailed                  ,
                                const char **subjects              ,
                                const char **froms                 ,
                                const char **tos                   ,
                                const char **urls                  )
{
    NOTIFY_START_FUNC;

    char nth[32];
    size_t x = 0;
    PurpleAccount *acct  = gc   ? purple_connection_get_account ( gc   ) : NULL;
    const char    *aname = acct ? purple_account_get_username   ( acct ) : NULL;
    const char    *proto = acct ? purple_account_get_protocol_id( acct ) : NULL;
    xmlnode       *email = xnode_new("alist");

    AL_NODE( alist, "messages"    , email );
    AL_PTR ( alist, "account-uid" , acct  );
    AL_STR ( alist, "account-name", aname );
    AL_STR ( alist, "im-protocol" , proto );
    
    for( x = 0; x < count; x++ )
    {
        xmlnode *mesg = xnode_new( "alist" );        
        snprintf( nth, sizeof(nth) - 1, "%ld", (long)x );
        MAYBE_ATTRIBUTE( subject, mesg );
        MAYBE_ATTRIBUTE( from   , mesg );
        MAYBE_ATTRIBUTE( to     , mesg );
        MAYBE_ATTRIBUTE( url    , mesg );
        AL_NODE( email, nth, mesg );
    }

    NOTIFY_CLOSE_FUNC( EMAILS, emails );
}

void *_elim_notify_formatted  ( const char *title                  ,
                                const char *primary                ,
                                const char *secondary              ,
                                const char *text                   )
{
    NOTIFY_START_FUNC;
    AL_STR ( alist, "title"     , title     );
    AL_STR ( alist, "primary"   , primary   );
    AL_STR ( alist, "secondary" , secondary );
    AL_STR ( alist, "text"      , text      );
    NOTIFY_CLOSE_FUNC( FORMATTED, formatted );
}

void *_elim_notify_search     ( PurpleConnection *gc               ,
                                const char *title                  ,
                                const char *primary                ,
                                const char *secondary              ,
                                PurpleNotifySearchResults *results ,
                                gpointer user_data                 )
{
    NOTIFY_START_FUNC;
    NOTIFY_CLOSE_FUNC( SEARCHRESULTS, search );
}

void _elim_notify_search_rows ( PurpleConnection *gc               ,
                                PurpleNotifySearchResults *results ,
                                void *data                         )
{

}

void *_elim_notify_userinfo   ( PurpleConnection *gc               ,
                                const char *who                    ,
                                PurpleNotifyUserInfo *user_info    )
{
    //NOTIFY_START_FUNC;
    //NOTIFY_CLOSE_FUNC( USERINFO, userinfo );
    return NULL;
}

void *_elim_notify_uri ( const char *uri )
{
    NOTIFY_START_FUNC;
    AL_STR ( alist, "url", uri );
    NOTIFY_CLOSE_FUNC( URI, uri );
}

void _elim_close_notify ( PurpleNotifyType type , void *ui_handle )
{
    CB_HANDLER  *handle = ui_handle;
    NOTIFY_RESP *resp   = handle ? handle->data : NULL;
    g_free( resp   );
    g_free( handle );
}
