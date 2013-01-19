/*
 * TableA_Destroy - VERSION: %I%  %E% %T%
 */
/*
 * TableA_Destroy - Destroy Table A by freeing all string pointers.
 * 
 *  022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void TableA_Destroy( void )

#else

void TableA_Destroy()

#endif
{
    extern TableA_t TableA[MAX_TABLE_A_ENTRIES];

#if TRACE_PRINT
    extern BUFR_Cntl_t BUFR_Cntl;
#endif

    int i;

#if TRACE_PRINT
    if( BUFR_TraceLevel() > 1 )
        fprintf(BUFR_Cntl.bufr_log, "Destroying Table A\n" );
#endif

    for( i=0; i < MAX_TABLE_A_ENTRIES; i++ )
        free( (void*) TableA[i] );
}
