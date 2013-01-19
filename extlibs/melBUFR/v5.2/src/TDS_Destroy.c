/*
 * TableD_Sequence_Destroy - VERSION: %I%  %E% %T%
 */
/*
 * TableD_Sequence_Destroy - Destroy the given Table D sequence (descriptor
 * list).
 *
 * NOTE: The sequence should be freed by the calling function.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void TableD_Sequence_Destroy( TableD_Sequence_t* DSeq )

#else

void TableD_Sequence_Destroy( DSeq )
TableD_Sequence_t* DSeq;

#endif
{
    TableD_Entry_t *ThisDE, *LastDE;

    if( DSeq == NULL || DSeq->head == NULL || DSeq->tail == NULL )
        return;

    /* Destroy each descriptor sequence list item. */

    ThisDE = DSeq->head->next;

    while( ThisDE != DSeq->tail )
    {
        LastDE = ThisDE;
        ThisDE = LastDE->next;
        free( (void*) LastDE );
    }

    free( (void*) DSeq->head );
    free( (void*) DSeq->tail );

    DSeq->num_entries = 0;
}
