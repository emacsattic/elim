#ifndef _EMACSIM_HANDLER_UTIL_H_
#define _EMACSIM_HANDLER_UTIL_H_

#include <purple.h>

PurpleConversation *find_conv_by_acct_uid  ( PurpleAccount *acct, gpointer id );
PurpleAccount      *find_acct_by_uid       ( gpointer uid );
PurplePlugin       *find_plugin_by_protocol( const char *name );

#endif
