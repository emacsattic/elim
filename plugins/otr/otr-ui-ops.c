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

/* libgcrypt headers */
#include <gcrypt.h>

/* libotr headers */
#include <libotr/privkey.h>

/* purple headers */
#include "util.h"
#include "account.h"
#include "notify.h"

/* purple-otr headers */
#include "otr-ui.h"
#include "otr.h"

/* internationalisation header */
#include <glib/gi18n-lib.h>

static const gchar *trust_states[] = { N_("Not private") ,
                                       N_("Unverified" ) ,
                                       N_("Private"    ) ,
                                       N_("Finished"   ) };

#if 0
static void account_changed_cb( PurpleAccount *acct ,
                                void          *data )
{
    const char *aname;
    const char *proto;
    char s[100];
    char *fingerprint;
    PurpleConnection *conn = NULL;

    if( acct ) 
    {
        char fingerprint_buf[45];
        aname = purple_account_get_username   ( acct );
        proto = purple_account_get_protocol_id( acct );
        conn  = purple_account_get_connection ( acct );
        fingerprint = otrl_privkey_fingerprint( otrg_plugin_userstate,
                                                fingerprint_buf,
                                                aname ,
                                                proto );
        if( fingerprint ) sprintf( s, _("Fingerprint: %.80s") , fingerprint );
        else              sprintf( s, _("No key present"    ) );
    }
    else 
    {
        sprintf(s, _("No account available"));
    }

    purple_notify_info( conn, aname, s, NULL );
}
#endif

/* Update the keylist, if it's visible */
static void otrui_op_update_keylist (void)
{
    gchar *titles[5];
    char   hash[45];
    ConnContext *context;
    Fingerprint *fingerprint;
    static const char const *header = "User\tState\tTrusted\tHash\tAccount\n";
    GString *message   = g_string_new( "" );


    for( context  = otrg_plugin_userstate->context_root;
         context != NULL;
         context  = context->next) 
    {
        PurplePlugin *p;
        char *proto_name;
        fingerprint = context->fingerprint_root.next;
        /* If there's no fingerprint, don't add it to the known
         * fingerprints list */
        while( fingerprint )
        {
            titles[0] = context->username;
            if( (context->msgstate           == OTRL_MSGSTATE_ENCRYPTED) &&
                (context->active_fingerprint != fingerprint            ) ) 
            {
                titles[1] = _("Unused");
            } 
            else 
            {
                titles[1] = (gchar *)
                  _(trust_states[otrg_plugin_context_to_trust(context)]);
            }

            titles[2]  = 
              (fingerprint->trust && fingerprint->trust[0])? _("Yes"): _("No");

            otrl_privkey_hash_to_human( hash, fingerprint->fingerprint );

            titles[3]  = hash;
            p          = purple_find_prpl( context->protocol );
            proto_name = (p && p->info->name) ? p->info->name : _("Unknown");
            titles[4]  =
              g_strdup_printf( "%s (%s)", context->accountname, proto_name );

            g_string_append_printf( message  , "%s\t%s\t%s\t%s\t%s\n",
                                    titles[0] ,
                                    titles[1] ,
                                    titles[2] ,
                                    titles[3] ,
                                    titles[4] );

            g_free( titles[4] );
            fingerprint = fingerprint->next;
        }
    }

    purple_notify_formatted( NULL,
                             "OTR Key List", header, NULL, message->str,
                             NULL, NULL );
    g_string_free( message, TRUE );
}

#if 0
static int fngsortval( Fingerprint *f )
{
    int is_active = 
      ( (f->context->msgstate           == OTRL_MSGSTATE_ENCRYPTED) &&
        (f->context->active_fingerprint == f) );
    TrustLevel level = otrg_plugin_context_to_trust( f->context );

    switch(level)
    {
      case TRUST_PRIVATE    : return is_active ? 0 : 100;
      case TRUST_UNVERIFIED : return is_active ? 1 : 100;
      case TRUST_FINISHED   : return 2;
      case TRUST_NOT_PRIVATE: return 3;
    }

    /* Shouldn't get here, but anyway. */
    return 200;
}
#endif

/* Initialize the OTR UI subsystem */
static void otrui_op_init(void)
{
    /* Nothing to do */
}

/* Deinitialize the OTR UI subsystem */
static void otrui_op_cleanup(void)
{
    /* Nothing to do */
}



static const OtrgUiUiOps ui_ops =
{
    otrui_op_init,
    otrui_op_cleanup,
    NULL,
    // otrui_op_update_fingerprint,
    otrui_op_update_keylist,
    NULL,
    NULL,
    //otrui_config_buddy,
    //otrui_get_prefs
};

/* Get the GTK UI ops */
const OtrgUiUiOps *otrui_get_ui_ops (void)
{
    return &ui_ops;
}
