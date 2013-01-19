/*
 * DataList_Size - VERSION: %I%  %E% %T%
 */
/*
 * DataList_Size() - Return the number of elements contained in a DataList_t.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int DataList_Size( DataList_t* DL )

#else

int DataList_Size( DL )
DataList_t* DL;

#endif
{
    DataEntry_t* de;
    int          size;

    if( DL == NULL )
        return 0;

    for( de=DL->head->next, size=0; de != DL->tail; de=de->next )
        size++;

    return size;
}
