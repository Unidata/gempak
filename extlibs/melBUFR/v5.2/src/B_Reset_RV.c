/*
 * BUFR_Reset_RefVals - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Reset_RefVals - Use descriptor 2-03-000 to restore ALL
 * redefined Table B reference values to their default values
 * by clearing all reference value stacks.
 * Return 1 on error, else 0.
 * 
 * CHANGE LOG
 * 
 *  022498 LAH:  Added prints to bufr_log file.
 * 
 */

#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;
#if TRACE_PRINT
    extern BUFR_Cntl_t BUFR_Cntl;
#endif
extern TableB_t* TableB;

#if PROTOTYPE_NEEDED

int BUFR_Reset_RefVals( void )

#else

int BUFR_Reset_RefVals()

#endif
{

    BUFR_Msg_t* BM;

    TableB_Entry_t* BE;
    ValStack_t*     vp;

    FXY_t fxy;

    BM = &BUFR_Msg;

#if TRACE_PRINT
    if( BUFR_TraceLevel() )
        fprintf(BUFR_Cntl.bufr_log, "Resetting all Table B reference values\n" );
#endif

    for( BE=TableB->head->next; BE != TableB->tail; BE=BE->next )
    {
        vp = &BE->item->RefValStack;
        while( ValStack_Pop( vp ) == 0 );
    }

    fxy = FXY_Pack( 2, 3, 0 );

    return DataList_Put( BM->data_list, &fxy, 1, NULL, 0 );
}
