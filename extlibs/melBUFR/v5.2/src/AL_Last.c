/*
 * AF_List_Last - VERSION: %I%  %E% %T%
 */
/*
 * AF_List_Last() - Return a pointer to the last element in a AF_List_t.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

AF_Entry_t* AF_List_Last( AF_List_t* AL )

#else

AF_Entry_t* AF_List_Last( AL )
AF_List_t* AL;

#endif
{
    register AF_Entry_t* AE;

    if( AL == NULL )
        return (AF_Entry_t*) NULL;

    for( AE=AL->head; AE->next != AL->tail; AE=AE->next )
        ;

    return AE;
}
