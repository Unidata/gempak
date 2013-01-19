/*
 * TableD_Destroy - VERSION: %I%  %E% %T%
 */
/*
 * TableD_Destroy - Destroy Table D by freeing all objects.
 * 
 * CHANGE LOG
 * 
 *  022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>
extern TableD_t* TableD;
#if TRACE_PRINT
    extern BUFR_Cntl_t BUFR_Cntl;
#endif

#if PROTOTYPE_NEEDED

void TableD_Destroy( void )

#else

void TableD_Destroy()

#endif
{

    TableD_Sequence_t *ThisDS, *LastDS;

    if( TableD == NULL || TableD->head == NULL || TableD->tail == NULL )
        return;

    /* Destroy each descriptor list item. */

#if TRACE_PRINT
    if( BUFR_TraceLevel() > 1 )
        fprintf(BUFR_Cntl.bufr_log, "Destroying Table D\n" );
#endif

    ThisDS = TableD->head->next;

    while( ThisDS != TableD->tail )
    {
        TableD_Sequence_Destroy( ThisDS );

        LastDS = ThisDS;
        ThisDS = ThisDS->next;

        free( (void*) LastDS );
    }

    /* free memory allocation for head and tail  (+ 3/5/96) */

    free( (void*) TableD->tail );
    free( (void*) TableD->head );
    free( (void*) TableD );
}
