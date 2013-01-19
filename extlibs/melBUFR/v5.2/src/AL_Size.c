/*
 * AF_List_Size - VERSION: %I%  %E% %T%
 */
/*
 * AF_List_Size() - Return the number of elements in an AF_List_t.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int AF_List_Size( AF_List_t* AL )

#else

int AF_List_Size( AL )
AF_List_t* AL;

#endif
{
    AF_Entry_t* AE;
    int         size;

    if( AL == NULL )
        return 0;

    for( AE=AL->head, size=0; AE->next != AL->tail; AE=AE->next )
        size++;

    return size;
}
