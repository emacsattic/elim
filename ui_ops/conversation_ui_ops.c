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
#include "conversation_ui_ops.h"
#include <string.h>

static void _elim_create_conversation  ( PurpleConversation *conv );
static void _elim_destroy_conversation ( PurpleConversation *conv );
static void _elim_write_chat           ( PurpleConversation *conv , 
                                         const char *who          ,
                                         const char *message      , 
                                         PurpleMessageFlags flags ,
                                         time_t mtime             );
static void _elim_write_im             ( PurpleConversation *conv    , 
                                         const char         *who     ,
                                         const char         *message , 
                                         PurpleMessageFlags flags    ,
                                         time_t             mtime    );
static void _elim_write_conv           ( PurpleConversation *conv    ,
                                         const char         *name    ,
                                         const char         *alias   ,
                                         const char         *message ,
                                         PurpleMessageFlags  flags   ,
                                         time_t mtime                );
static void _elim_chat_add_users       ( PurpleConversation *conv         ,
                                         GList              *cbuddies     ,
                                         gboolean            new_arrivals );
static void _elim_chat_rename_user     ( PurpleConversation *conv      , 
                                         const char         *old_name  ,
                                         const char         *new_name  , 
                                         const char         *new_alias );
static void _elim_chat_remove_users    ( PurpleConversation *conv  , 
                                         GList              *users );
static void _elim_chat_update_user     ( PurpleConversation *conv , 
                                         const char         *user );
static void _elim_present              ( PurpleConversation *conv );
static void _elim_custom_smiley_write  ( PurpleConversation *conv  , 
                                         const char         *smile ,
                                         const guchar       *data  , 
                                         gsize               size  );
static void _elim_custom_smiley_close  ( PurpleConversation *conv  , 
                                         const char         *smile );
static void _elim_send_confirm         ( PurpleConversation *conv  , 
                                         const char *message       );
static gboolean _elim_has_focus        ( PurpleConversation *conv );
static gboolean _elim_custom_smiley_add( PurpleConversation *conv  , 
                                         const char         *smile , 
                                         gboolean           remote );

PurpleConversationUiOps elim_conversation_ui_ops =
{
    _elim_create_conversation  ,
    _elim_destroy_conversation ,
    _elim_write_chat           ,
    _elim_write_im             ,
    _elim_write_conv           ,
    _elim_chat_add_users       ,
    _elim_chat_rename_user     ,
    _elim_chat_remove_users    ,
    _elim_chat_update_user     ,
    _elim_present              ,
    _elim_has_focus            ,
    _elim_custom_smiley_add    ,
    _elim_custom_smiley_write  ,
    _elim_custom_smiley_close  ,
    _elim_send_confirm         ,
    NULL      ,
    NULL      ,
    NULL      ,
    NULL
};


static gboolean _elim_strippable ( PurpleConversation   *conv  , 
                                   PurpleMessageFlags    flag  )
{
    PurpleConnectionFlags feat = purple_conversation_get_features( conv );
    PurpleAccount        *acct = purple_conversation_get_account ( conv );
    const char          *proto = purple_account_get_protocol_id  ( acct );
    // µblog plugin gets this wrong:
    if( strstr( (proto ? proto : "") , "-mbpurple-" ) ) 
        feat = feat|PURPLE_CONNECTION_HTML;

    return ( (feat & PURPLE_CONNECTION_HTML) &&
             (flag & PURPLE_MESSAGE_RECV   ) && 
            !(flag & PURPLE_MESSAGE_RAW    )  ) ? TRUE : FALSE;
}

static void _elim_conv_args ( xmlnode *alist, PurpleConversation *conv )
{
    PurpleAccount *acct  = purple_conversation_get_account( conv );
    const char    *aname = purple_account_get_username    ( acct );
    const char    *proto = purple_account_get_protocol_id ( acct );
    const char    *title = purple_conversation_get_title  ( conv );
    const char    *cname = purple_conversation_get_name   ( conv );
    PurpleConnectionFlags  cflag = purple_conversation_get_features( conv );
    PurpleConversationType ctype = purple_conversation_get_type    ( conv );
    
    AL_STR( alist, "account-name" , aname );
    AL_STR( alist, "im-protocol"  , proto );
    AL_INT( alist, "account-uid"  , (int)acct );
    AL_INT( alist, "conv-uid"     , (int)conv );
    AL_STR( alist, "conv-name"    , cname );
    AL_STR( alist, "conv-title"   , title ? title : cname );
    AL_INT( alist, "conv-type"    , ctype );
    AL_INT( alist, "conv-features", cflag );    
}

static void _elim_create_conversation  ( PurpleConversation *conv )
{
    char    *ID    = new_elim_id();
    xmlnode *args  = xnode_new( "alist" );
    xmlnode *mcall = func_call( "elim-conv-create", ID, args );
    g_free( ID );
    _elim_conv_args( args, conv );
    add_outbound_sexp( mcall );
}

static void _elim_destroy_conversation ( PurpleConversation *conv )
{
    char    *ID    = new_elim_id();
    xmlnode *args  = xnode_new( "alist" );
    xmlnode *mcall = func_call( "elim-conv-destroy", ID, args );
    g_free( ID );
    _elim_conv_args( args, conv );
    add_outbound_sexp( mcall );
}

static void _elim_write_chat ( PurpleConversation *conv    , 
                               const char         *who     ,
                               const char         *message , 
                               PurpleMessageFlags  flags   ,
                               time_t              mtime   )
{
    char    *msg   = NULL;
    char    *ID    = new_elim_id();
    xmlnode *args  = xnode_new( "alist" );
    xmlnode *mcall = func_call( "elim-conv-write-chat", ID, args );
    g_free( ID );

    fprintf( stderr, "(elim-debug _elim_write_chat)\n" );
    
    if( _elim_strippable( conv, flags ) )
        msg = purple_markup_strip_html( message );

    _elim_conv_args( args, conv );
    AL_STR( args, "who"  , who ? who : ""      );
    AL_STR( args, "text" , msg ? msg : message );
    AL_INT( args, "flags", flags   );
    AL_INT( args, "time" , mtime   );

    if( msg ) g_free( msg );
    add_outbound_sexp( mcall );
}

static void _elim_write_im ( PurpleConversation *conv    , 
                             const char         *who     ,
                             const char         *message , 
                             PurpleMessageFlags flags    ,
                             time_t mtime                )
{
    char    *msg   = NULL;
    char    *ID    = new_elim_id();
    xmlnode *args  = xnode_new( "alist" );
    xmlnode *mcall = func_call( "elim-conv-write-im", ID, args );
    g_free( ID );

    fprintf( stderr, "(elim-debug _elim_write_im 0)\n" );

    if( _elim_strippable( conv, flags ) )
        msg = purple_markup_strip_html( message );

    fprintf( stderr, "(elim-debug _elim_write_im 1)\n" );

    _elim_conv_args( args, conv );

    fprintf( stderr, "(elim-debug _elim_write_im 2)\n" );
    AL_STR( args, "who"  , who ? who : ""      );
    fprintf( stderr, "(elim-debug _elim_write_im 3)\n" );
    AL_STR( args, "text" , msg ? msg : message );
    fprintf( stderr, "(elim-debug _elim_write_im 4)\n" );
    AL_INT( args, "flags", flags   );
    fprintf( stderr, "(elim-debug _elim_write_im 5)\n" );
    AL_INT( args, "time" , mtime   );
    fprintf( stderr, "(elim-debug _elim_write_im 6)\n" );

    if( msg ) g_free( msg );
    fprintf( stderr, "(elim-debug _elim_write_im 7)\n" );
    add_outbound_sexp( mcall );
}

static void _elim_write_conv ( PurpleConversation *conv    ,
                               const char         *name    ,
                               const char         *alias   ,
                               const char         *message ,
                               PurpleMessageFlags  flags   ,
                               time_t mtime                )
{
    char    *msg   = NULL;
    char    *ID    = new_elim_id();
    xmlnode *args  = xnode_new( "alist" );
    xmlnode *mcall = func_call( "elim-conv-write-sys", ID, args );
    g_free( ID );

    fprintf( stderr, "(elim-debug _elim_write_conv)\n" );

    if( _elim_strippable( conv, flags ) )
        msg = purple_markup_strip_html( message );

    _elim_conv_args( args, conv );
    AL_STR( args, "who"  , (name  ? name  : (alias ? alias : "")) );
    AL_STR( args, "alias", (alias ? alias : (name  ? name  : "")) );
    AL_STR( args, "text" , msg ? msg : message );
    AL_INT( args, "flags", flags   );
    AL_INT( args, "time" , mtime   );

    if( msg ) g_free( msg );
    add_outbound_sexp( mcall );
}

static void _elim_chat_add_users       ( PurpleConversation *conv         ,
                                         GList              *cbuddies     ,
                                         gboolean            new_arrivals )
{
    char    *ID    = new_elim_id();
    xmlnode *args  = xnode_new( "alist" );
    xmlnode *mcall = func_call( "elim-chat-add-users", ID, args );
    g_free( ID );

    _elim_conv_args( args, conv );
    AL_BOOL( args, "new_arrivals" , new_arrivals );

    for( ; cbuddies; cbuddies = cbuddies->next )
    {
        xmlnode             *cbuddy = xnode_new( "alist" );
        PurpleConvChatBuddy *pccb   = cbuddies->data;
        const char          *name   = pccb->name ? pccb->name : "";
        AL_STR ( cbuddy, "name"     , pccb->name      ? pccb->name      : "" );
        AL_STR ( cbuddy, "alias"    , pccb->alias     ? pccb->alias     : "" );
        AL_STR ( cbuddy, "alias-key", pccb->alias_key ? pccb->alias_key : "" );
        AL_BOOL( cbuddy, "on-blist" , pccb->buddy );
        AL_INT ( cbuddy, "flags"    , pccb->flags );
        AL_NODE( args  , name , cbuddy );
    }

    add_outbound_sexp( mcall );     
}
static void _elim_chat_rename_user     ( PurpleConversation *conv      , 
                                         const char         *old_name  ,
                                         const char         *new_name  , 
                                         const char         *new_alias ){}
static void _elim_chat_remove_users    ( PurpleConversation *conv  , 
                                         GList              *users ){}
static void _elim_chat_update_user     ( PurpleConversation *conv, 
                                         const char         *user ){}
static void _elim_present              ( PurpleConversation *conv )
{
    char    *ID    = new_elim_id();
    xmlnode *args  = xnode_new( "alist" );
    xmlnode *mcall = func_call( "elim-conv-present", ID, args );
    g_free( ID );

    _elim_conv_args( args, conv );
    add_outbound_sexp( mcall ); 
}
static void _elim_custom_smiley_write  ( PurpleConversation *conv  , 
                                         const char         *smile ,
                                         const guchar       *data  , 
                                         gsize               size  ){}
static void _elim_custom_smiley_close  ( PurpleConversation *conv  , 
                                         const char         *smile ){}
static void _elim_send_confirm         ( PurpleConversation *conv  , 
                                         const char *message       ){}

static gboolean _elim_has_focus        ( PurpleConversation *conv )
{
    return TRUE;
}

static gboolean _elim_custom_smiley_add( PurpleConversation *conv  , 
                                         const char         *smile , 
                                         gboolean           remote )
{
    return TRUE;
}

