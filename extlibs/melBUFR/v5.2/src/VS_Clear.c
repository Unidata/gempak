/*
 * ValStack_Clear - VERSION: %I%  %E% %T%
 */
/*
 * ValStack_Clear - Clear all entries on a value stack.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void ValStack_Clear( ValStack_t* VS )

#else

void ValStack_Clear( VS )
ValStack_t* VS;

#endif
{
    ValEntry_t *ThisVE, *NextVE;

    if( VS == NULL )
        return;

    if( VS->head == NULL || VS->tail == NULL )      /* Stack is empty */
        return;

    /* Clear each stack item. */

    ThisVE = VS->head->next;

    while( ThisVE != VS->tail )
    {
        NextVE = ThisVE->next;
        free( (void*) ThisVE );
        ThisVE = NextVE;
    }

    VS->head->next = VS->tail;
}
