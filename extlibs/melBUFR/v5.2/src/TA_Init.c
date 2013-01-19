/*
 * TableA_Init - VERSION: %I%  %E% %T%
 */
/*
 * TableA_Init - Allocate and initialize Table A
 * 
 * 
 *  022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void TableA_Init( void )

#else

void TableA_Init()

#endif
{
    extern TableA_t TableA[MAX_TABLE_A_ENTRIES];
#if TRACE_PRINT
    extern BUFR_Cntl_t BUFR_Cntl;
#endif

#if TRACE_PRINT
    if( BUFR_TraceLevel() > 1 )
        fprintf(BUFR_Cntl.bufr_log, "Initializing Table A\n" );
#endif

    memset( (char*)TableA, 0, sizeof(TableA_t)*MAX_TABLE_A_ENTRIES );
}
