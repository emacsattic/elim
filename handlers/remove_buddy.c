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
#include "remove_buddy.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"


xmlnode * _h_elim_remove_buddy ( const char *name ,
                                 const char *id   ,
                                 SEXP_VALUE *args ,
                                 gpointer data    )
{
    ASSERT_ALISTP( args, id, name );

    elim_ping();
    
    const char *aname = ALIST_VAL_STR( args, "account-name" );
    const char *proto = ALIST_VAL_STR( args, "im-protocol"  );
    gpointer    auid  = ALIST_VAL_PTR( args, "account-uid"  );

    PurpleAccount *acct = 
      auid ? find_acct_by_uid( auid ) : purple_accounts_find( aname, proto );

    if( !acct )
    {
        sexp_val_free( args );
        return response_error( ENXIO, id, name, "unknown account" );
    }

    const char  *b_arg = ALIST_VAL_STRING( args, "buddy-name" );
    const char  *bname = purple_normalize( acct, b_arg        );
    const char  *gname = ALIST_VAL_STRING( args, "group"      );
    PurpleGroup *group = (gname && *gname) ? purple_find_group( gname ) : NULL;
    PurpleBuddy *buddy = ( group ? 
                           purple_find_buddy_in_group( acct, bname, group ) :
                           purple_find_buddy         ( acct, bname        ) );

    // buddy must be in our local blist or libpurple won't remove it 
    // from the server list (determined empirically and by inspecting code)
    if( !buddy )
    {
        buddy = purple_buddy_new( acct, bname, bname );
        purple_blist_add_buddy  ( buddy, NULL, NULL, NULL );
    }

    if( buddy )
    {
        // the order of the remove operations is important: it has to be
        // this way round, as noted above: account buddy removal won't 
        // happen if the buddy is not in the blist when we try:
        if( !group ) group = purple_buddy_get_group( buddy );
        purple_account_remove_buddy( acct, buddy, group );
        purple_blist_remove_buddy  ( buddy );
    }
    else 
    {
        sexp_val_free( args );
        return response_error( ENXIO, id, name, "no such buddy" );
    }


    xmlnode *rval = xnode_new( "alist" );
    AL_STR( rval, "account-name", purple_account_get_username   ( acct ) );
    AL_STR( rval, "im-protocol" , purple_account_get_protocol_id( acct ) );
    AL_PTR( rval, "account-uid" , acct  );
    AL_PTR( rval, "buddy-uid"   , buddy );

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
