/*
 * Table0_Destroy - VERSION: %I%  %E% %T%
 */
/*
 * Table0_Destroy - Destroy Table 0 by freeing all string pointers.
 * 
 * CHANGE LOG
 * 
 *  022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void Table0_Destroy( void )

#else

void Table0_Destroy()

#endif
{
    extern Table0_t Table0[MAX_TABLE_0_ENTRIES];

#if TRACE_PRINT
    extern BUFR_Cntl_t BUFR_Cntl;
#endif

    int       i;
    Table0_t* tp;

#if TRACE_PRINT
    if( BUFR_TraceLevel() > 1 )
        fprintf(BUFR_Cntl.bufr_log, "Destroying Table 0\n" );
#endif

    for( i=0, tp=&Table0[0]; i < MAX_TABLE_0_ENTRIES; i++, tp++ )
        free( (void*) tp->name );
}
