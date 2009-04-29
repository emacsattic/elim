#include "idle_ui_ops.h"
#include <time.h>

static time_t when = 0;

PurpleIdleUiOps elim_idle_ui_ops = 
{
    elim_idle ,
    NULL      ,
    NULL      ,
    NULL      ,
    NULL
};


time_t elim_idle (void) { return time(NULL) - when; }
time_t elim_ping (void) { return when = time(NULL); }
