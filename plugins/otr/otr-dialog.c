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


/* system headers */
#include <stdlib.h>

/* pidgin headers */
#include "notify.h"

/* libotr headers */
#include <libotr/proto.h>
#include <libotr/message.h>

/* pidgin-otr headers */
#include "otr-dialog.h"
#include "otr.h"

static const OtrgDialogUiOps *ui_ops = NULL;

/* Set the UI ops */
void otrg_dialog_set_ui_ops( const OtrgDialogUiOps *ops )
{
    ui_ops = ops;
}

/* Get the UI ops */
const OtrgDialogUiOps *otrg_dialog_get_ui_ops( void )
{
    return ui_ops;
}

/* Initialize the OTR dialog subsystem */
void otrg_dialog_init ( void )
{
    ui_ops->init();
}

/* Deinitialize the OTR dialog subsystem */
void otrg_dialog_cleanup( void )
{
    ui_ops->cleanup();
}

void otrg_dialog_notify_message( PurpleNotifyMsgType type,
                                 const char *aname   , 
                                 const char *proto   , 
                                 const char *user    ,
                                 const char *title   , 
                                 const char *header  , 
                                 const char *message )
{
    char *head = g_strdup_printf( "%s (%s/%s) %s", user, aname, proto, header );
    purple_notify_message( NULL, type, title, header, message, NULL, NULL );
    g_free( head );
}



void otrg_dialog_notify_error( const char *aname   ,
                               const char *proto   ,
                               const char *user    , 
                               const char *title   , 
                               const char *header  ,
                               const char *message )
{
    otrg_dialog_notify_message( PURPLE_NOTIFY_MSG_ERROR, 
                                aname, proto, user, title, header, message );
}

void otrg_dialog_notify_warning( const char *aname   ,
                                 const char *proto   ,
                                 const char *user    , 
                                 const char *title   , 
                                 const char *header  ,
                                 const char *message )
{
    otrg_dialog_notify_message( PURPLE_NOTIFY_MSG_WARNING, 
                                aname, proto, user, title, header, message );
}

void otrg_dialog_notify_info( const char *aname   ,
                              const char *proto   ,
                              const char *user    , 
                              const char *title   , 
                              const char *header  ,
                              const char *message )
{
    otrg_dialog_notify_message( PURPLE_NOTIFY_MSG_INFO, 
                                aname, proto, user, title, header, message );
}

/* Display an OTR control message for the given accountname / protocol /
 * username conversation.  Return 0 on success, non-0 on error (in which
 * case the message will be displayed inline as a received message). */
int otrg_dialog_display_otr_message( const char *aname ,
                                     const char *proto , 
                                     const char *user  , 
                                     const char *mesg  )
{
    return ui_ops->display_otr_message( aname, proto, user, mesg );
}


OtrgDialogWaitHandle otrg_dialog_private_key_wait_start( const char *acct  ,
                                                         const char *proto )
{
    return ui_ops->private_key_wait_start( acct, proto );
}

void otrg_dialog_private_key_wait_done( OtrgDialogWaitHandle handle )
{
    ui_ops->private_key_wait_done( handle );
}


/* Show a dialog informing the user that a correspondent (who) has sent
 * us a Key Exchange Message (kem) that contains an unknown fingerprint. */
void otrg_dialog_unknown_fingerprint( OtrlUserState us  , 
                                      const char *aname ,
                                      const char *proto , 
                                      const char *who   , 
                                      unsigned char fingerprint[20] )
{
    ui_ops->unknown_fingerprint( us, aname, proto, who, fingerprint );
}

/* Show a dialog asking the user to verify the given fingerprint. */
void otrg_dialog_verify_fingerprint ( Fingerprint *fprint )
{
    ui_ops->verify_fingerprint( fprint );
}

/* Show a dialog asking the user to give an SMP secret. */
void otrg_dialog_socialist_millionaires( ConnContext *context )
{
    ui_ops->socialist_millionaires( context, NULL, TRUE );
}

/* Show a dialog asking the user to give an SMP secret, prompting with a
 * question. */
void otrg_dialog_socialist_millionaires_q( ConnContext *context,
                                           char *question )
{
    ui_ops->socialist_millionaires( context, question, TRUE );
}

/* Update the status of an ongoing socialist millionaires protocol. */
void otrg_dialog_update_smp( ConnContext *context, double progress_level )
{
    ui_ops->update_smp( context, progress_level );
}

/* Call this when a context transitions to ENCRYPTED. */
void otrg_dialog_connected( ConnContext *context )
{
    ui_ops->connected( context );
}

/* Call this when a context transitions to PLAINTEXT. */
void otrg_dialog_disconnected( ConnContext *context )
{
    ui_ops->disconnected( context );
}

/* Call this when we receive a Key Exchange message that doesn't cause
 * our state to change (because it was just the keys we knew already). */
void otrg_dialog_stillconnected( ConnContext *context )
{
    ui_ops->stillconnected( context );
}

/* Call this if the remote user terminates his end of an ENCRYPTED
 * connection, and lets us know. */
void otrg_dialog_finished( const char *aname ,
                           const char *proto ,
                           const char *user  )
{
    ui_ops->finished( aname, proto, user );
}

/* Set all OTR buttons to "sensitive" or "insensitive" as appropriate.
 * Call this when accounts are logged in or out. */
void otrg_dialog_resensitize_all (void)
{
    ui_ops->resensitize_all();
}

/* Set up the per-conversation information display */
void otrg_dialog_new_conv( PurpleConversation *conv )
{
    ui_ops->new_conv( conv );
}

/* Remove the per-conversation information display */
void otrg_dialog_remove_conv( PurpleConversation *conv )
{
    ui_ops->remove_conv( conv );
}
