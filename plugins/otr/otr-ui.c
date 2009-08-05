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

#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* libgcrypt headers */
#include <gcrypt.h>

/* purple headers */
#include "notify.h"
#include "version.h"
#include "util.h"
#include "debug.h"

#ifdef WIN32
/* On Win32, include win32dep.h from pidgin for correct definition
 * of LOCALEDIR */
#include "win32dep.h"
#endif /* WIN32 */

/* elim-otr headers */
#include "otr-dialog.h"
#include "otr-ui.h"
#include "otr.h"

/* internationalisation header */
#include <glib/gi18n-lib.h>

/* libotr headers */
#include <libotr/privkey.h>
#include <libotr/proto.h>
#include <libotr/tlv.h>
#include <libotr/message.h>
#include <libotr/userstate.h>

#define CALL_UI_OP_A(name, ...) \
    if( ui_ops && ui_ops->name ) { ui_ops->name(__VA_ARGS__)

#define CALL_UI_OP_Z }

#define CALL_UI_OP(name, ...) CALL_UI_OP_A(name, __VA_ARGS__); CALL_UI_OP_Z

#define CALL_UI_OP_AND_STOP(name, ...) \
    CALL_UI_OP_A(name, __VA_ARGS__); return; CALL_UI_OP_Z

static const OtrgUiUiOps *ui_ops = NULL;

/* Set the UI ops */
void otrg_ui_set_ui_ops( const OtrgUiUiOps *ops )
{
    ui_ops = ops;
}

/* Get the UI ops */
const OtrgUiUiOps *otrg_ui_get_ui_ops (void)
{
    return ui_ops;
}

/* Initialize the OTR UI subsystem */
void otrg_ui_init (void)
{
    CALL_UI_OP( init );
}

/* Deinitialize the OTR UI subsystem */
void otrg_ui_cleanup (void)
{
    CALL_UI_OP( cleanup );
}

/* Call this function when the DSA key is updated; it will redraw the
 * UI, if visible. */
void otrg_ui_update_fingerprint (void)
{
    CALL_UI_OP( update_fingerprint );
}

/* Update the keylist, if it's visible */
void otrg_ui_update_keylist (void)
{
    CALL_UI_OP( update_keylist );
}

/* Send an OTR Query Message to attempt to start a connection */
void otrg_ui_connect_connection( ConnContext *context )
{
    /* Send an OTR Query to the other side. */
    PurpleAccount *acct;
    char *msg;
	
    /* Don't do this if we're already ENCRYPTED */
    if( !context || context->msgstate == OTRL_MSGSTATE_ENCRYPTED )
        return;
	
    acct = purple_accounts_find( context->accountname, context->protocol );

    if( !acct ) 
    {
        PurplePlugin *p = purple_find_prpl( context->protocol );
        const char *txt = (p && p->info->name) ? p->info->name : _("Unknown");
        msg = g_strdup_printf(_("Account %s (%s) could not be found"),
                              context->accountname, txt );
        otrg_dialog_notify_error( context->accountname,
                                  context->protocol   ,
                                  context->username   ,
                                  _("Account not found"), msg, NULL );
        g_free( msg );
        return;
    }
    otrg_plugin_send_default_query( context, acct );	
}

/* Drop a context to PLAINTEXT state */
void otrg_ui_disconnect_connection( ConnContext *context )
{
    /* Don't do anything with PLAINTEXT fingerprints */
    if( !context || context->msgstate == OTRL_MSGSTATE_PLAINTEXT )
        return;
		
    otrg_plugin_disconnect  ( context );
    otrg_dialog_disconnected( context );
}

/* Forget a fingerprint */
void otrg_ui_forget_fingerprint( Fingerprint *fingerprint )
{
    ConnContext *context;
	
    if( !fingerprint ) return;

    /* Don't do anything with the active fingerprint if we're in the
     * ENCRYPTED state. */
    context = fingerprint->context;

    if( context->msgstate           == OTRL_MSGSTATE_ENCRYPTED &&
	    context->active_fingerprint == fingerprint             )
        return;
	
    otrl_context_forget_fingerprint( fingerprint, 1 );
    otrg_plugin_write_fingerprints ();
    otrg_ui_update_keylist ();
}

/* Configure OTR for a particular buddy */
void otrg_ui_config_buddy( PurpleBuddy *buddy )
{
    CALL_UI_OP( config_buddy, buddy );
}

/* Load the preferences for a particular account / username */
void otrg_ui_get_prefs( OtrgUiPrefs   *prefsp,
                        PurpleAccount *acct  ,
                        const char    *name  )
{
    /* Check to see if the protocol for this account supports OTR at all. */
    const char *proto = purple_account_get_protocol_id( acct );

    if( !otrg_plugin_proto_supports_otr( proto ) )
    {
        prefsp->policy            = OTRL_POLICY_NEVER;
        prefsp->avoid_logging_otr = FALSE;
        prefsp->show_otr_button   = FALSE;
        return;
    }

    CALL_UI_OP_AND_STOP( get_prefs, prefsp, acct, name );

    /* If we've got no other way to get the prefs, use sensible defaults */
    prefsp->policy            = OTRL_POLICY_DEFAULT;
    prefsp->avoid_logging_otr = FALSE;
    prefsp->show_otr_button   = FALSE;
}

