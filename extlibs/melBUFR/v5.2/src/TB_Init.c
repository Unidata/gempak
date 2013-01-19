/*
 * TableB_Init - VERSION: %I%  %E% %T%
 */
/*
 * TableB_Init - Allocate and initialize Table B's head and tail.
 * 
 * CHANGE LOG
 * 
 *  022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int TableB_Init( void )

#else

int TableB_Init()

#endif
{
    extern TableB_t* TableB;
#if TRACE_PRINT
    extern BUFR_Cntl_t BUFR_Cntl;
#endif

#if TRACE_PRINT
    if( BUFR_TraceLevel() > 1 )
        fprintf(BUFR_Cntl.bufr_log, "Initializing Table B\n" );
#endif

    if( (TableB = (TableB_t*) malloc( sizeof(TableB_t))) == NULL )
        return 1;

    TableB->head = (TableB_Entry_t*) malloc( sizeof(TableB_Entry_t) );

    if( TableB->head == NULL )
    {
        free( (void*) TableB );
        return 1;
    }

    TableB->tail = (TableB_Entry_t*) malloc( sizeof(TableB_Entry_t) );

    if( TableB->tail == NULL )
    {
        free( (void*) TableB->head );
        free( (void*) TableB );
        return 1;
    }

    TableB->head->item = (Descriptor_t*) NULL;
    TableB->head->next = TableB->tail;

    TableB->tail->item = (Descriptor_t*) NULL;
    TableB->tail->next = (TableB_Entry_t*) NULL;

    return 0;
}
