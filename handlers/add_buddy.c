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
#include "add_buddy.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"


xmlnode * _h_elim_add_buddy ( const char *name ,
                              const char *id   ,
                              SEXP_VALUE *args ,
                              gpointer data    )
{
    ASSERT_ALISTP( args, id, name );

    elim_ping();
    
    int         loopc = 0;
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

    const char *b_arg = ALIST_VAL_STRING( args, "bnode-name" );
    const char *bname = purple_normalize( acct, b_arg        );
    const char *gname = ALIST_VAL_STRING( args, "group"      );
    if( !gname || !*gname ) gname = "Buddies";

    PurpleGroup *group = purple_group_new( gname );
    PurpleBuddy *buddy = purple_buddy_new( acct, bname, b_arg );
    PurpleBuddy *clone = NULL;
    //fprintf( stderr, "add-buddy( b: %p, g: %p )\n", buddy, group );
    // remove other references to this buddy
    purple_blist_add_buddy  ( buddy, NULL, group, NULL );
    purple_account_add_buddy( acct , buddy );
    while( ( clone = (PurpleBuddy*)find_blist_node_clone( buddy ) ) )
    {
        if( loopc++ > 99   ) 
        {
            fprintf( stderr, "ARGH! clone reaping looped: %d\n", loopc );
            break;
        }
        if( clone == buddy ) 
        { 
            fprintf( stderr, "ARGH! %p not a clone of %p\n", buddy, clone );
            break;
        }
        fprintf( stderr, "(removing clone %p %ld (of buddy: %p)\n", 
                 clone, (long)clone, buddy );
        fprintf( stderr, "   name : %s\n", purple_buddy_get_name(clone) );
        fprintf( stderr, "   group: %s)\n", 
                 purple_group_get_name( purple_buddy_get_group(clone) ) );
        purple_blist_remove_buddy( clone );
    }

    xmlnode *rval = xnode_new( "alist" );
    AL_PTR( rval, "account-uid" , acct  );
    AL_PTR( rval, "bnode-uid"   , buddy );
    AL_PTR( rval, "group-uid"   , group );
    AL_STR( rval, "bnode-name"  , purple_buddy_get_name         ( buddy ) );
    AL_STR( rval, "bnode-alias" , purple_buddy_get_alias        ( buddy ) );
    AL_STR( rval, "account-name", purple_account_get_username   ( acct  ) );
    AL_STR( rval, "im-protocol" , purple_account_get_protocol_id( acct  ) );
    AL_STR( rval, "group-name"  , purple_group_get_name         ( group ) );

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
