/*
 * DataList_Last - VERSION: %I%  %E% %T%
 */
/*
 * DataList_Last() - Return a pointer to the last element in a DataList_t.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

DataEntry_t* DataList_Last( DataList_t* DL )

#else

DataEntry_t* DataList_Last( DL )
DataList_t* DL;

#endif
{
    register DataEntry_t* de;

    if( DL == NULL )
        return (DataEntry_t*) NULL;

    for( de=DL->head; de->next != DL->tail; de=de->next )
        ;

    return de;
}
