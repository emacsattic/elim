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
#include "request_ui_ops.h"

static void *_elim_request_input ( const char         *title         ,
                                   const char         *primary       ,
                                   const char         *secondary     ,
                                   const char         *default_value ,
                                   gboolean            multiline     ,
                                   gboolean            masked        ,
                                   gchar              *hint          ,
                                   const char         *ok_text       ,
                                   GCallback           ok_cb         ,
                                   const char         *cancel_text   ,
                                   GCallback           cancel_cb     ,
                                   PurpleAccount      *account       ,
                                   const char         *who           ,
                                   PurpleConversation *conv          ,
                                   void               *user_data     );

static void *_elim_request_choice( const char         *title         ,
                                   const char         *primary       ,
                                   const char         *secondary     ,
                                   int                 default_value ,
                                   const char         *ok_text       ,
                                   GCallback           ok_cb         ,
                                   const char         *cancel_text   ,
                                   GCallback           cancel_cb     ,
                                   PurpleAccount      *account       ,
                                   const char         *who           ,
                                   PurpleConversation *conv          ,
                                   void               *user_data     ,
                                   va_list             choices       );

static void *_elim_request_action( const char          *title        ,
                                   const char          *primary      ,
                                   const char          *secondary    ,
                                   int                  default_act  ,
                                   PurpleAccount       *account      ,
                                   const char          *who          ,
                                   PurpleConversation  *conv         ,
                                   void                *user_data    ,
                                   size_t               action_count ,
                                   va_list              actions      );

static void *_elim_request_fields( const char          *title        ,
                                   const char          *primary      ,
                                   const char          *secondary    ,
                                   PurpleRequestFields *fields       ,
                                   const char          *ok_text      ,
                                   GCallback            ok_cb        ,
                                   const char          *cancel_text  ,
                                   GCallback            cancel_cb    ,
                                   PurpleAccount       *account      ,
                                   const char          *who          ,
                                   PurpleConversation  *conv         ,
                                   void                *user_data    );

static void *_elim_request_file  ( const char            *title      ,
                                   const char            *filename   ,
                                   gboolean               savedialog ,
                                   GCallback              ok_cb      ,
                                   GCallback              cancel_cb  ,
                                   PurpleAccount         *account    ,
                                   const char            *who        ,
                                   PurpleConversation    *conv       ,
                                   void                  *user_data  );

static void _elim_close_request  ( PurpleRequestType type, void *ui_handle );

static void *_elim_request_folder( const char            *title      ,
                                   const char            *dirname    ,
                                   GCallback              ok_cb      ,
                                   GCallback              cancel_cb  ,
                                   PurpleAccount         *account    ,
                                   const char            *who        ,
                                   PurpleConversation    *conv       ,
                                   void                  *user_data  );

PurpleRequestUiOps elim_request_ui_ops =
{
    _elim_request_input  ,
    _elim_request_choice ,
    _elim_request_action ,
    _elim_request_fields ,
    _elim_request_file   ,
    _elim_close_request  ,
    _elim_request_folder ,
    NULL ,
    NULL ,
    NULL ,
    NULL
};

typedef void (*action_func) (gpointer data);
typedef struct _REQ_RESP REQ_RESP;
struct _REQ_RESP
{
    char             *id   ;
    gpointer          data ;
    PurpleRequestType type ;
    union    
    {
        struct { PurpleRequestInputCb  ok; PurpleRequestInputCb  nok; } input ;
        struct { PurpleRequestChoiceCb ok; PurpleRequestChoiceCb nok; } choice;
        struct { action_func        *func; int                 count; } action;
        struct { PurpleRequestFileCb   ok; PurpleRequestFileCb   nok; } path  ;
    } req;
};

static xmlnode * _elim_request_input_cb ( gpointer ptr, SEXP_VALUE *args )
{
    REQ_RESP *handle = ptr;
    if( handle ) 
    {
        gpointer data = handle->data;

        if( args && (args->type == SEXP_ALIST) )
        {
            int status = ALIST_VAL_INT( args, "status" );
            if( status == 0 ) 
            {
                char *input = ALIST_VAL_STR( args, "input" );
                ( input ? 
                  handle->req.input.ok  : 
                  handle->req.input.nok )( data, input );
            }
            else { (handle->req.input.nok)( data, "" ); }
        }
        else { handle->req.input.nok( data, "" ); }
    }
    
    if( handle ) g_free( handle );
    if( args   ) sexp_val_free( args );

    return NULL;
}

static void *_elim_request_input ( const char         *title         ,
                                   const char         *primary       ,
                                   const char         *secondary     ,
                                   const char         *default_value ,
                                   gboolean            multiline     ,
                                   gboolean            masked        ,
                                   gchar              *hint          ,
                                   const char         *ok_text       ,
                                   GCallback           ok_cb         ,
                                   const char         *cancel_text   ,
                                   GCallback           cancel_cb     ,
                                   PurpleAccount      *account       ,
                                   const char         *who           ,
                                   PurpleConversation *conv          ,
                                   void               *user_data     )
{
    CB_HANDLER *cbh   = g_new0( CB_HANDLER, 1 );
    REQ_RESP   *resp  = g_new0( REQ_RESP  , 1 );
    xmlnode    *alist = xnode_new( "alist" );
    char       *ID    = new_elim_id();

    AL_STR ( alist, "title"     , title         );
    AL_STR ( alist, "primary"   , primary       );
    AL_STR ( alist, "secondary" , secondary     );
    AL_STR ( alist, "default"   , default_value );
    AL_STR ( alist, "hint"      , hint          );
    AL_STR ( alist, "ok_label"  , ok_text       );
    AL_STR ( alist, "nok_label" , cancel_text   );
    AL_BOOL( alist, "multi-line", multiline     );
    AL_BOOL( alist, "secret"    , masked        );

    if( account )
    {
        const char *aname = purple_account_get_username   ( account );
        const char *proto = purple_account_get_protocol_id( account );
        AL_INT ( alist, "account-uid" , (int)account );
        AL_STR ( alist, "account-name", aname        );
        AL_STR ( alist, "im-protocol" , proto        );
    }

    if( conv )
    {
        const char            *title = purple_conversation_get_title   ( conv );
        const char            *cname = purple_conversation_get_name    ( conv );
        PurpleConnectionFlags  cflag = purple_conversation_get_features( conv );
        PurpleConversationType ctype = purple_conversation_get_type    ( conv );

        AL_PTR ( alist, "conv-uid"     , conv  );
        AL_STR ( alist, "conv-name"    , cname );
        AL_STR ( alist, "conv-title"   , title );
        AL_ENUM( alist, "conv-type"    , ctype , ":conversation-type" );
        AL_ENUM( alist, "conv-features", cflag , ":connection-flags"  );
    }

    resp->req.input.ok   = (PurpleRequestInputCb)ok_cb;
    resp->req.input.nok  = (PurpleRequestInputCb)cancel_cb;
    resp->data = user_data;
    resp->type = PURPLE_REQUEST_INPUT;
    resp->id   = ID;
    cbh ->func = _elim_request_input_cb;
    cbh ->data = resp;
    store_cb_data( ID, cbh );
    xmlnode *mcall = func_call( "elim-request-input", ID, alist );
    add_outbound_sexp( mcall );
    return cbh;
}

static xmlnode * _elim_request_choice_cb ( gpointer ptr, SEXP_VALUE *args )
{
    REQ_RESP *handle = ptr;
    if( handle ) 
    {
        gpointer data = handle->data;

        if( args && (args->type == SEXP_ALIST) )
        {
            int status = ALIST_VAL_INT( args, "status" );
            if( status == 0 ) 
            {
                int choice = ALIST_VAL_INT( args, "choice" );
                ( (choice != -1) ? 
                  handle->req.choice.ok  : 
                  handle->req.choice.nok )( data, choice );
            }
            else { (handle->req.choice.nok)( data, 0 ); }
        }
        else { handle->req.choice.nok( data, 0 ); }
    }
    
    if( handle ) g_free( handle );
    if( args   ) sexp_val_free( args );

    return NULL;
}

static void *_elim_request_choice( const char         *title         ,
                                   const char         *primary       ,
                                   const char         *secondary     ,
                                   int                 default_value ,
                                   const char         *ok_text       ,
                                   GCallback           ok_cb         ,
                                   const char         *cancel_text   ,
                                   GCallback           cancel_cb     ,
                                   PurpleAccount      *account       ,
                                   const char         *who           ,
                                   PurpleConversation *conv          ,
                                   void               *user_data     ,
                                   va_list             choices       )
{
    CB_HANDLER *cbh    = g_new0( CB_HANDLER, 1 );
    REQ_RESP   *resp   = g_new0( REQ_RESP  , 1 );
    xmlnode    *alist  = xnode_new( "alist" );
    char       *ID     = new_elim_id();   
    xmlnode    *choice = xnode_new( "alist" );

    VA_LIST_TO_ALIST( choice, INT, const char *, int, choices );

    AL_STR ( alist, "title"     , title         );
    AL_STR ( alist, "primary"   , primary       );
    AL_STR ( alist, "secondary" , secondary     );
    AL_STR ( alist, "ok_label"  , ok_text       );
    AL_STR ( alist, "nok_label" , cancel_text   );   
    AL_STR ( alist, "who"       , who           );
    AL_INT ( alist, "default"   , default_value );
    AL_NODE( alist, "choices"   , choice        );

    if( account )
    {
        const char *aname = purple_account_get_username   ( account );
        const char *proto = purple_account_get_protocol_id( account );
        AL_INT ( alist, "account-uid" , (int)account );
        AL_STR ( alist, "account-name", aname        );
        AL_STR ( alist, "im-protocol" , proto        );
    }

    if( conv )
    {
        const char            *title = purple_conversation_get_title   ( conv );
        const char            *cname = purple_conversation_get_name    ( conv );
        PurpleConnectionFlags  cflag = purple_conversation_get_features( conv );
        PurpleConversationType ctype = purple_conversation_get_type    ( conv );

        AL_PTR ( alist, "conv-uid"     , conv  );
        AL_STR ( alist, "conv-name"    , cname );
        AL_STR ( alist, "conv-title"   , title );
        AL_ENUM( alist, "conv-type"    , ctype , ":conversation-type" );
        AL_ENUM( alist, "conv-features", cflag , ":connection-flags"  );
    }

    resp->req.choice.ok   = (PurpleRequestChoiceCb)ok_cb;
    resp->req.choice.nok  = (PurpleRequestChoiceCb)cancel_cb;
    resp->data = user_data;
    resp->type = PURPLE_REQUEST_CHOICE;
    resp->id   = ID;
    cbh ->func = _elim_request_choice_cb;
    cbh ->data = resp;
    store_cb_data( ID, cbh );
    xmlnode *mcall = func_call( "elim-request-choice", ID, alist );
    add_outbound_sexp( mcall );
    return cbh;
}

static xmlnode * _elim_request_action_cb( gpointer ptr, SEXP_VALUE *args )
{
    REQ_RESP *handle = ptr;
    if( handle )
    {
        gpointer data = handle->data;
        
        if( args && (args->type == SEXP_ALIST) )
        {
            int status = ALIST_VAL_INT( args, "status" );
            int max    = handle->req.action.count;
            if( status == 0 ) 
            {
                int x = 0;
                gpointer     f = (gpointer)ALIST_VAL_INT( args, "choice" );
                action_func *F = handle->req.action.func;
                for( x = 0; x < max; x++ )
                    if( f == *(F++) ) { (*F)( data ); break; }
            }
            else { (handle->req.input.nok)( data, "" ); }
        }
    }

    if( handle->type == PURPLE_REQUEST_ACTION )
        g_free( handle->req.action.func );

    if( handle ) g_free       ( handle );
    if( args   ) sexp_val_free( args   );

    return NULL;
}

static void *_elim_request_action( const char          *title        ,
                                   const char          *primary      ,
                                   const char          *secondary    ,
                                   int                  default_act  ,
                                   PurpleAccount       *account      ,
                                   const char          *who          ,
                                   PurpleConversation  *conv         ,
                                   void                *user_data    ,
                                   size_t               action_count ,
                                   va_list              actions      )
{
    CB_HANDLER *cbh   = g_new0( CB_HANDLER, 1 );
    REQ_RESP   *resp  = g_new0( REQ_RESP  , 1 );
    xmlnode    *alist = xnode_new( "alist" );
    char       *ID    = new_elim_id();
    xmlnode    *acts  = xnode_new( "alist" );
    resp->req.action.func  = g_new0( action_func, action_count );
    resp->req.action.count = action_count;

    if( actions )
    {
        int offs = 0;
        const char *label = NULL;
        while( (label = va_arg( actions, const char *)) )
        {
            action_func func = va_arg( actions, action_func );
            AL_PTR( acts, label, func );
            if( offs < action_count )
                *(resp->req.action.func + offs++) = func;
        }
    }

    AL_STR ( alist, "title"     , title       );
    AL_STR ( alist, "primary"   , primary     );
    AL_STR ( alist, "secondary" , secondary   );
    AL_STR ( alist, "who"       , who         );
    AL_INT ( alist, "default"   , default_act );
    AL_NODE( alist, "actions"   , acts        );

    if( account )
    {
        const char *aname = purple_account_get_username   ( account );
        const char *proto = purple_account_get_protocol_id( account );
        AL_INT ( alist, "account-uid" , (int)account );
        AL_STR ( alist, "account-name", aname        );
        AL_STR ( alist, "im-protocol" , proto        );
    }

    if( conv )
    {
        const char            *title = purple_conversation_get_title   ( conv );
        const char            *cname = purple_conversation_get_name    ( conv );
        PurpleConnectionFlags  cflag = purple_conversation_get_features( conv );
        PurpleConversationType ctype = purple_conversation_get_type    ( conv );

        AL_PTR ( alist, "conv-uid"     , conv  );
        AL_STR ( alist, "conv-name"    , cname );
        AL_STR ( alist, "conv-title"   , title );
        AL_ENUM( alist, "conv-type"    , ctype , ":conversation-type" );
        AL_ENUM( alist, "conv-features", cflag , ":connection-flags"  );
    }

    resp->data = user_data;
    resp->type = PURPLE_REQUEST_ACTION;
    resp->id   = ID;
    cbh ->func = _elim_request_action_cb;
    cbh ->data = resp;
    store_cb_data( ID, cbh );
    xmlnode *mcall = func_call( "elim-request-action", ID, alist );
    add_outbound_sexp( mcall );
    return cbh;
}

static void *_elim_request_fields( const char          *title        ,
                                   const char          *primary      ,
                                   const char          *secondary    ,
                                   PurpleRequestFields *fields       ,
                                   const char          *ok_text      ,
                                   GCallback            ok_cb        ,
                                   const char          *cancel_text  ,
                                   GCallback            cancel_cb    ,
                                   PurpleAccount       *account      ,
                                   const char          *who          ,
                                   PurpleConversation  *conv         ,
                                   void                *user_data    )
{
    return NULL;
}

static xmlnode * _elim_request_path_cb( gpointer ptr, SEXP_VALUE *args )
{
    REQ_RESP *handle = ptr;
    if( handle ) 
    {
        gpointer data = handle->data;

        if( args && (args->type == SEXP_ALIST) )
        {
            int status = ALIST_VAL_INT( args, "status" );
            if( status == 0 ) 
            {
                char *path = ALIST_VAL_STR( args, "path" );
                ( path ? 
                  handle->req.path.ok  : 
                  handle->req.path.nok )( data, path );
            }
            else { (handle->req.path.nok)( data, "" ); }
        }
        else { handle->req.path.nok( data, "" ); }
    }
    
    if( handle ) g_free( handle );
    if( args   ) sexp_val_free( args );

    return NULL;
}

static void *_elim_request_file  ( const char            *title      ,
                                   const char            *filename   ,
                                   gboolean               savedialog ,
                                   GCallback              ok_cb      ,
                                   GCallback              cancel_cb  ,
                                   PurpleAccount         *account    ,
                                   const char            *who        ,
                                   PurpleConversation    *conv       ,
                                   void                  *user_data  )
{
    CB_HANDLER *cbh   = g_new0( CB_HANDLER, 1 );
    REQ_RESP   *resp  = g_new0( REQ_RESP  , 1 );
    xmlnode    *alist = xnode_new( "alist" );
    char       *ID    = new_elim_id();

    AL_STR ( alist, "title"  , title      );
    AL_STR ( alist, "default", filename   );
    AL_STR ( alist, "who"    , who        );
    AL_BOOL( alist, "savep"  , savedialog );

    if( account )
    {
        const char *aname = purple_account_get_username   ( account );
        const char *proto = purple_account_get_protocol_id( account );
        AL_PTR ( alist, "account-uid" , account );
        AL_STR ( alist, "account-name", aname   );
        AL_STR ( alist, "im-protocol" , proto   );
    }

    if( conv )
    {
        const char            *title = purple_conversation_get_title   ( conv );
        const char            *cname = purple_conversation_get_name    ( conv );
        PurpleConnectionFlags  cflag = purple_conversation_get_features( conv );
        PurpleConversationType ctype = purple_conversation_get_type    ( conv );

        AL_PTR ( alist, "conv-uid"     , conv  );
        AL_STR ( alist, "conv-name"    , cname );
        AL_STR ( alist, "conv-title"   , title );
        AL_ENUM( alist, "conv-type"    , ctype , ":conversation-type" );
        AL_ENUM( alist, "conv-features", cflag , ":connection-flags"  );
    }

    resp->req.path.ok  = (PurpleRequestFileCb)ok_cb;
    resp->req.path.nok = (PurpleRequestFileCb)cancel_cb;
    resp->data         = user_data;
    resp->type         = PURPLE_REQUEST_FILE;
    resp->id           = ID;

    cbh ->func = _elim_request_path_cb;
    cbh ->data = resp;

    store_cb_data( ID, cbh );
    xmlnode *mcall = func_call( "elim-request-file", ID, alist );
    add_outbound_sexp( mcall );
    return cbh;    
}

static void _elim_close_request  ( PurpleRequestType type, void *ui_handle )
{
    CB_HANDLER *cbh  = ui_handle;
    REQ_RESP   *resp = cbh->data;

    if( resp->type == PURPLE_REQUEST_ACTION )
        g_free( resp->req.action.func );
    g_free( resp );
    g_free( cbh  );
}

static void *_elim_request_folder( const char            *title      ,
                                   const char            *dirname    ,
                                   GCallback              ok_cb      ,
                                   GCallback              cancel_cb  ,
                                   PurpleAccount         *account    ,
                                   const char            *who        ,
                                   PurpleConversation    *conv       ,
                                   void                  *user_data  )
{
    CB_HANDLER *cbh   = g_new0( CB_HANDLER, 1 );
    REQ_RESP   *resp  = g_new0( REQ_RESP  , 1 );
    xmlnode    *alist = xnode_new( "alist" );
    char       *ID    = new_elim_id();

    AL_STR ( alist, "title"  , title   );
    AL_STR ( alist, "default", dirname );
    AL_STR ( alist, "who"    , who     );

    if( account )
    {
        const char *aname = purple_account_get_username   ( account );
        const char *proto = purple_account_get_protocol_id( account );
        AL_PTR ( alist, "account-uid" , account );
        AL_STR ( alist, "account-name", aname   );
        AL_STR ( alist, "im-protocol" , proto   );
    }

    if( conv )
    {
        const char            *title = purple_conversation_get_title   ( conv );
        const char            *cname = purple_conversation_get_name    ( conv );
        PurpleConnectionFlags  cflag = purple_conversation_get_features( conv );
        PurpleConversationType ctype = purple_conversation_get_type    ( conv );
        AL_PTR ( alist, "conv-uid"     , conv  );
        AL_STR ( alist, "conv-name"    , cname );
        AL_STR ( alist, "conv-title"   , title );
        AL_ENUM( alist, "conv-type"    , ctype , ":conversation-type" );
        AL_ENUM( alist, "conv-features", cflag , ":connection-flags"  );
    }

    resp->req.path.ok  = (PurpleRequestFileCb)ok_cb;
    resp->req.path.nok = (PurpleRequestFileCb)cancel_cb;
    resp->data         = user_data;
    resp->type         = PURPLE_REQUEST_FOLDER;
    resp->id           = ID;

    cbh ->func = _elim_request_path_cb;
    cbh ->data = resp;

    store_cb_data( ID, cbh );
    xmlnode *mcall = func_call( "elim-request-directory", ID, alist );
    add_outbound_sexp( mcall );
    return cbh;    
}
