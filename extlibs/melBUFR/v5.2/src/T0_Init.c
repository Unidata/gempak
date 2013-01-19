/*
 * Table0_Init - VERSION: %I%  %E% %T%
 */
/*
 * Table0_Init - Initialize Table 0
 * 
 * CHANGE LOG
 * 
 *  022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void Table0_Init( void )

#else

void Table0_Init()

#endif
{
    extern Table0_t Table0[MAX_TABLE_0_ENTRIES];

#if TRACE_PRINT
    extern BUFR_Cntl_t BUFR_Cntl;
#endif

#if TRACE_PRINT
    if( BUFR_TraceLevel() > 1 )
        fprintf(BUFR_Cntl.bufr_log, "Initializing Table 0\n" );
#endif

    memset( (char*)Table0, 0, sizeof(Table0_t)*MAX_TABLE_0_ENTRIES );
}
