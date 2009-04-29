#ifndef _ELIM_IDLE_UI_OPS_H_
#define _ELIM_IDLE_UI_OPS_H_

#include <purple.h>

extern PurpleIdleUiOps elim_idle_ui_ops;

time_t elim_ping (void);
time_t elim_idle (void);

#endif
