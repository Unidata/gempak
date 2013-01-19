/*
 * TableB_Destroy - VERSION: %I%  %E% %T%
 */
/*
 * TableB_Destroy - Destroy Table B by freeing all objects.
 */

/* ************************************************************************
*
*      MODIFICATION LOG
*
**************************************************************************
*
*     2/25/96
*     BY Victor (CAST)
*        ValStack_Destroy code was moved to above frees to eleminate
*         memory leak.  Need to deallocate elements of structure
*         before deallocating structure.
*
*     3/05/1996
*     BY LOUIS HEMBREE
*     Added code to free memory allocation for Table B and corresponding
*     structure for head and tail of linked list
*
 *  022498 LAH:  Added prints to bufr_log file.
***************************************************************************/

#include <mel_bufr.h>
extern TableB_t* TableB;
#if TRACE_PRINT
    extern BUFR_Cntl_t BUFR_Cntl;
#endif

#if PROTOTYPE_NEEDED

void TableB_Destroy( void )

#else

void TableB_Destroy()

#endif
{

    TableB_Entry_t *ThisBE, *LastBE;

    if( TableB == NULL || TableB->head == NULL || TableB->tail == NULL )
        return;

    /* Destroy each descriptor list item. */

#if TRACE_PRINT
    if( BUFR_TraceLevel() > 1 )
        fprintf(BUFR_Cntl.bufr_log, "Destroying Table B\n" );
#endif

    ThisBE = TableB->head->next;

    while( ThisBE != TableB->tail )
    {
        /* Destroy redefined reference value stack. */
        /* This code was moved to above following frees to eleminate */
        /* memory leak.  Need to deallocate elements of structure  */
        /* before deallocating structure  (+2 2/25/96) */

        (void) ValStack_Destroy( &ThisBE->item->RefValStack );

        /* Destroy descriptor (item). */

        free( (void*) ThisBE->item->units );
        free( (void*) ThisBE->item->description );
        free( (void*) ThisBE->item );

        /* Deallocate this descriptor. */

        LastBE = ThisBE;
        ThisBE = LastBE->next;
        free( (void*) LastBE );
    }

    /* free memory allocation for head and tail of structure (+ 3/5/96) */

    free( (void*) TableB->head );
    free( (void*) TableB->tail );
    free( (void*) TableB );
}
