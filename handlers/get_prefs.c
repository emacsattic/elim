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
#include "get_prefs.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

#define MAYBE_IGNORE_PREF( n )                      \
     { if( !strcmp( (n), "/pidgin"      ) ) return; \
       if( !strcmp( (n), "/plugins/gtk" ) ) return; }

static void _add_pref_data( xmlnode *node, const char *name )
{
    xmlnode    *entry = NULL;
    PurplePrefType pt = purple_prefs_get_type( name );
    GList      *vlist = NULL;

    MAYBE_IGNORE_PREF( name );

    entry = xnode_new( "alist" );

    AL_NODE( node , name, entry );
    AL_ENUM( entry, "pref-type", pt, ":pref-type" );

    switch( pt )
    {
      case PURPLE_PREF_BOOLEAN:
        AL_BOOL( entry, "pref-value", purple_prefs_get_bool  ( name ) );
        break;
      case PURPLE_PREF_INT    :
        AL_INT ( entry, "pref-value", purple_prefs_get_int   ( name ) );
        break;
      case PURPLE_PREF_STRING :
        AL_STR ( entry, "pref-value", purple_prefs_get_string( name ) );
        break;
      case PURPLE_PREF_PATH   :
        AL_STR ( entry, "pref-value", purple_prefs_get_path  ( name ) );
        break;
      case PURPLE_PREF_STRING_LIST:
        vlist = purple_prefs_get_string_list( name );
      case PURPLE_PREF_PATH_LIST  :
        vlist = purple_prefs_get_path_list  ( name );
      break;
      case PURPLE_PREF_NONE   :
        break;
      default:
        fprintf( stderr, "pref: %s has type %lu (unsupportd)\n", name, 
                 (unsigned long)pt );
    }

    if( vlist )
    {
        GList   *item = NULL;
        xmlnode *list = xnode_new( "list" );
        AL_NODE( entry, "pref-value", list );
        for( item = vlist; item; item = item->next )
        {
            char    *str    = item->data;
            xmlnode *string = xnode_new( "string" );
            xnode_insert_data ( string, str, -1 );
            xnode_insert_child( list  , string  );
            g_free( str );
        }
        g_list_free( vlist );
    }

    GList *labels = purple_prefs_get_children_names( name );
    GList *label  = NULL;

    if( labels )
    {
        xmlnode *kids = xnode_new( "alist" );
        AL_NODE( entry, "pref-children", kids );
        for( label = labels; label; label = label->next )
        {
            char *n = label->data;
            _add_pref_data( kids, n );
            g_free( n );
        }
        g_list_free( labels );
    }
}

xmlnode * _h_elim_get_prefs ( const char *name , 
                              const char *id   ,
                              SEXP_VALUE *args , 
                              gpointer    data )
{
    elim_ping();
    
    const char *pref_name = 
      ( ( args && (args->type == SEXP_ALIST) ) ? 
        ALIST_VAL_STR( args, "pref-name" ) : "/" );
    
    if( !pref_name || !*pref_name )
        pref_name = "/";

    xmlnode *rval  = xnode_new( "alist" );
    xmlnode *prefs = xnode_new( "alist" );

    AL_NODE( rval, "prefs", prefs );

    _add_pref_data( prefs, "/" );

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
