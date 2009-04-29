#include "core_ui_ops.h"

static void        _elim_ui_prefs_init ( void );
static void        _elim_debug_ui_init ( void ); /* Unfortunate necessity. */
static void        _elim_ui_init       ( void );
static void        _elim_quit          ( void );
static GHashTable* _elim_get_ui_info   ( void );

PurpleCoreUiOps elim_core_ui_ops = 
{
    _elim_ui_prefs_init ,
    _elim_debug_ui_init ,
    _elim_ui_init       ,
    _elim_quit          ,
    _elim_get_ui_info   ,
    NULL                ,
    NULL                ,
    NULL
};


// most of these are unnecessary
static void        _elim_ui_prefs_init ( void ){}
static void        _elim_debug_ui_init ( void ){} 
static void        _elim_ui_init       ( void ){}
static void        _elim_quit          ( void ){}
static GHashTable* _elim_get_ui_info   ( void ){ return NULL; }
