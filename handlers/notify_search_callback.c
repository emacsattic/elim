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
#include "notify_search_callback.h"
#include "../elim-rpc.h"
#include "../prpl/util.h"

#define PA     PurpleAccount
#define PC     PurpleConnection
#define PNSB   PurpleNotifySearchButton
#define PNSRCB PurpleNotifySearchResultsCallback 
#define PNSR   PurpleNotifySearchResults

xmlnode * _h_elim_notify_search_callback ( const char *name , 
                                           const char *id   , 
                                           SEXP_VALUE *args , 
                                           gpointer    data )
{
    GList       *item = NULL;
    xmlnode     *rval = NULL;
    CB_HANDLER  *cbh  = check_cb_data( id );
    NOTIFY_RESP *resp = cbh ? cbh->data : NULL;

    if( ( (cbh ->type) == CB_TYPE_NOTIFY_SEARCH       ) &&
        ( (resp->type) == PURPLE_NOTIFY_SEARCHRESULTS )  )
    {
        GList *row  = NULL;
        PNSB  *btn  = NULL;
        PNSR  *res  = resp->sres;
        PA    *auid = ALIST_VAL_PTR( args, "account-uid" );
        PNSRCB cbid = ALIST_VAL_PTR( args, "callback"    );
        gint   ridx = ALIST_VAL_INT( args, "row-index"   );
        PA    *acct = find_acct_by_uid( auid );
        PC    *gc   = purple_account_get_connection( acct );
        gint   i;

        if( gc && res )
        {
            for( i = 0, item = res->rows; item; item = item->next, i++ )
                if( i == ridx ) { row = item->data; break; }
            
            for( item = res->buttons; item; item = item->next )
                if( ((PNSB *)item->data)->callback == cbid )
                {
                    btn = item->data;
                    break;
                }
        }
        
        if( row && btn )
            ( btn->callback )( gc, row, resp->user_data );
    }

    sexp_val_free( args );
    return rval;
}