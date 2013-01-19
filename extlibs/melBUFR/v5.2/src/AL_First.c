/*
 * AF_List_First - VERSION: %I%  %E% %T%
 */
/*
 * AF_List_First() - Return a pointer to the first element in an AF_List_t.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

AF_Entry_t* AF_List_First( AF_List_t* AL )

#else

AF_Entry_t* AF_List_First( AL )
AF_List_t* AL;

#endif
{
    if( AL == NULL )
        return (AF_Entry_t*) NULL;
    else 
        return AL->head->next;
}
