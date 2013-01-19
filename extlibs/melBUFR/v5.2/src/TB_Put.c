/*
 * TableB_Put - VERSION: %I%  %E% %T%
 */
/*
 * TableB_Put - Add a descriptor to the end of the list of Table B
 * descriptors.
 * Return 1 on error, else 0.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int TableB_Put( Descriptor_t ItemToAdd )

#else

int TableB_Put( ItemToAdd )
Descriptor_t ItemToAdd;

#endif
{
    extern TableB_t* TableB;

    TableB_Entry_t *NewBE, *LastBE, *ThisBE;
    Descriptor_t   *NewD;

    if( (NewBE=(TableB_Entry_t*)malloc(sizeof(TableB_Entry_t))) == NULL )
        return 1;

    if( (NewD = (Descriptor_t*) malloc(sizeof(Descriptor_t))) == NULL )
    {
        free( (void*) NewBE );
        return 1;
    }

    /* Copy contents of given descriptor to newly allocated one. */

    *NewD = ItemToAdd;

    NewBE->item = NewD;
    NewBE->next = TableB->head->next;

    /* Descriptor is initialized.  Add it before the tail in Table B. */

    LastBE = TableB->head;
    ThisBE = TableB->head->next;

    while( ThisBE != TableB->tail )
    {
        LastBE = ThisBE;
        ThisBE = ThisBE->next;
    }

    LastBE->next = NewBE;
    NewBE->next  = TableB->tail;

    return 0;
}
