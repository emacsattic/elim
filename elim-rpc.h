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
#ifndef _EMACS_IM_HANDLER_H_
#define _EMACS_IM_HANDLER_H_

#include <glib.h>
#include <stdio.h>
#include <errno.h>
#include <purple.h>

#include "xnode/xnode.h"
#include "sexp/sexp-util.h"

// ==========================================================================

#define ASSERT_ALISTP(s,i,n)                                                   \
    if( !(s) ) return response_error( EINVAL, (i), (n), "no args received" );  \
    if( (s)->type != SEXP_ALIST )                                              \
    {                                                                          \
        sexp_val_free( s );                                                    \
        return response_error( EINVAL, (i), (n), "arg value is not an alist" );\
    }

#define AL_STR(a,n,v)    \
     xnode_insert_child( (a), xnode_alist_item_string ( (n), (v) ) )

#define AL_PTR(a,n,v)    \
     xnode_insert_child( (a), xnode_alist_item_integer( (n), (long)(v) ) )

#define AL_INT(a,n,v)    \
     xnode_insert_child( (a), xnode_alist_item_integer( (n), (long)(v) ) )

#define AL_BOOL(a,n,v)   \
     xnode_insert_child( (a), xnode_alist_item_boolean( (n), (v) ) )

#define AL_NODE(a,n,v)   \
     xnode_insert_child( (a), xnode_alist_item_xnode  ( (n), (v) ) )

#define AL_ENUM(a,n,v,t) \
     xnode_insert_child( (a), xnode_alist_item_enum   ( (n), (v), (t) ) )

// ==========================================================================

typedef xmlnode *(*CB_FUNC)(gpointer data, SEXP_VALUE *args);
typedef struct _CB_HANDLER CB_HANDLER;
struct _CB_HANDLER { CB_FUNC func; gpointer data; };

// ==========================================================================

xmlnode * response_error( int         code    ,
                          const char *id      ,
                          const char *name    ,
                          const char *message );


xmlnode * response_value( int         code  ,
                          const char *id    ,
                          const char *name  ,
                          xmlnode    *value );

xmlnode * func_call( const char *name, const char *id, xmlnode *args );

char * new_elim_id ();

gboolean store_cb_data( char *key, gpointer value );
gpointer fetch_cb_data( const char *key );


#endif
