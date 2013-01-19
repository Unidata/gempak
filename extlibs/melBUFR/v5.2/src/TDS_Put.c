/*
 * TableD_Sequence_Put - VERSION: %I%  %E% %T%
 */
/*
 * TableD_Sequence_Put - Add a Table D entry to a given Table D descriptor
 * sequence.
 *
 * NOTE: The address of the corresponding Table B entry isn't initialized by
 *       this function. That will be done by another function after all
 *       Table D descriptors have been collected.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int TableD_Sequence_Put( TableD_Sequence_t* DSeq, FXY_t FXYval )

#else

int TableD_Sequence_Put( DSeq, FXYval )
TableD_Sequence_t* DSeq;
FXY_t              FXYval;

#endif
{
    TableD_Entry_t *NewDE, *ThisDE, *LastDE;

    if( DSeq == NULL )
        return 1;

    NewDE = (TableD_Entry_t*) malloc( sizeof(TableD_Entry_t) );

    if( NewDE == NULL )
        return 1;

    NewDE->fxy_value = FXYval;

    /*
     * Descriptor sequences must be in sequential order. Find the last one
     * in order to add an entry after it.
     */

    LastDE = DSeq->head;
    ThisDE = DSeq->head->next;

    while( ThisDE != DSeq->tail )   /* Find tail */
    {
        LastDE = ThisDE;
        ThisDE = ThisDE->next;
    }

    LastDE->next = NewDE;
    NewDE->next  = DSeq->tail;

    DSeq->num_entries++;

    return 0;
}
