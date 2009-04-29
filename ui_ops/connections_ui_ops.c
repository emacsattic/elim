#include "connections_ui_ops.h"

static void _elim_connect_progress        ( PurpleConnection *gc ,
                                            const char     *text ,
                                            size_t          step ,
                                            size_t    step_count );
static void _elim_connected               ( PurpleConnection *gc );
static void _elim_disconnected            ( PurpleConnection *gc );
static void _elim_notice                  ( PurpleConnection *gc , 
                                            const char     *text );
static void _elim_network_connected       ( void );
static void _elim_network_disconnected    ( void );
static void _elim_report_disconnect_reason( PurpleConnection     *gc     ,
                                            PurpleConnectionError reason ,
                                            const char           *text   );

PurpleConnectionUiOps elim_connections_ui_ops = 
{
    _elim_connect_progress        ,
    _elim_connected               ,
    _elim_disconnected            ,
    _elim_notice                  ,
    _elim_notice                  ,
    _elim_network_connected       ,
    _elim_network_disconnected    ,
    _elim_report_disconnect_reason,
    NULL ,
    NULL ,
    NULL
};

static void _elim_notice ( PurpleConnection *conn, const char *msg )
{
    PurpleAccount *acct = purple_connection_get_account( conn );
    if( acct )
    {
        char       *ID    = new_elim_id();
        xmlnode    *alist = xnode_new( "alist" );
        xmlnode    *mcall = func_call( "elim-connection-status", ID, alist );
        const char *aname = purple_account_get_username   ( acct );
        const char *proto = purple_account_get_protocol_id( acct );
        int         state = purple_connection_get_state   ( conn );
        g_free( ID );
        AL_INT( alist, "account-uid" , (int)acct );
        AL_STR( alist, "account-name", aname     );
        AL_STR( alist, "im-protocol" , proto     );
        AL_INT( alist, "state"       , state     );
        AL_STR( alist, "message"     , msg       );
        add_outbound_sexp( mcall );
    }
}

static void _elim_connect_progress        ( PurpleConnection *gc ,
                                            const char     *text ,
                                            size_t          step ,
                                            size_t    step_count )
{
    PurpleAccount *acct = purple_connection_get_account( gc );
    if( acct )
    {
        char       *ID    = new_elim_id();
        xmlnode    *alist = xnode_new( "alist" );
        xmlnode    *mcall = func_call( ID, "elim-connection-progress", alist );
        const char *aname = purple_account_get_username   ( acct );
        const char *proto = purple_account_get_protocol_id( acct );
        int         state = purple_connection_get_state   ( gc   );
        g_free( ID );
        AL_INT( alist, "account-uid" , (int)acct  );
        AL_STR( alist, "account-name", aname      );
        AL_STR( alist, "im-protocol" , proto      );
        AL_INT( alist, "state"       , state      );
        AL_INT( alist, "step"        , state      );
        AL_INT( alist, "step-count"  , step_count );        
        AL_STR( alist, "message"     , text       );
        add_outbound_sexp( mcall );
    }

}

static void _elim_connected ( PurpleConnection *gc )
{
    _elim_notice( gc, "connected" );
}

static void _elim_disconnected ( PurpleConnection *gc )
{
    _elim_notice( gc, "disconnected" );
}

static void _elim_network    ( const char *call )
{
    char       *ID    = new_elim_id();
    xmlnode    *mcall = func_call( call , ID, NULL );
    g_free( ID );
    add_outbound_sexp( mcall );
}

static void _elim_network_connected   () { _elim_network("elim-network-up"  ); }
static void _elim_network_disconnected() { _elim_network("elim-network-down"); }

static void _elim_report_disconnect_reason( PurpleConnection     *conn   ,
                                            PurpleConnectionError reason ,
                                            const char           *text   )
{
    PurpleAccount *acct = purple_connection_get_account( conn );
    if( acct )
    {
        char       *ID    = new_elim_id();
        xmlnode    *alist = xnode_new( "alist" );
        xmlnode    *mcall = func_call( "elim-disconnect-reason", ID, alist );
        const char *aname = purple_account_get_username   ( acct );
        const char *proto = purple_account_get_protocol_id( acct );
        int         state = purple_connection_get_state   ( conn );
        g_free( ID );
        AL_INT( alist, "account-uid" , (int)acct );
        AL_STR( alist, "account-name", aname     );
        AL_STR( alist, "im-protocol" , proto     );
        AL_INT( alist, "state"       , state     );
        AL_INT( alist, "reason-code" , reason    );
        AL_STR( alist, "message"     , text      );
        add_outbound_sexp( mcall );
    }
}
