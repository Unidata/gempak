/*
 * TableD_Init - VERSION: %I%  %E% %T%
 */
/*
 * TableD_Init - Initialize Table D
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

int TableD_Init( void )

#else

int TableD_Init()

#endif
{

#if TRACE_PRINT
    if( BUFR_TraceLevel() > 1 )
        fprintf(BUFR_Cntl.bufr_log, "Initializing Table D\n" );
#endif

    if( (TableD = (TableD_t*) malloc( sizeof(TableD_t))) == NULL )
        return 1;

    TableD->head = (TableD_Sequence_t*)malloc(sizeof(TableD_Sequence_t));

    if( TableD->head == NULL )
    {
        free( (void*) TableD );
        return 1;
    }

    TableD->tail = (TableD_Sequence_t*)malloc(sizeof(TableD_Sequence_t));

    if( TableD->tail == NULL )
    {
        free( (void*) TableD->head );
        free( (void*) TableD );
        return 1;
    }

    /* Create empty list of descriptor sequences. */

    TableD->head->head = (TableD_Entry_t*) NULL;
    TableD->head->tail = (TableD_Entry_t*) NULL;
    TableD->head->next = TableD->tail;

    TableD->tail->head = (TableD_Entry_t*) NULL;
    TableD->tail->tail = (TableD_Entry_t*) NULL;
    TableD->tail->next = (TableD_Sequence_t*) NULL;

    return 0;
}
