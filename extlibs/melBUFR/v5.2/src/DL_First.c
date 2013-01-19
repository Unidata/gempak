/*
 * DataList_First - VERSION: %I%  %E% %T%
 */
/*
 * DataList_First() - Return a pointer to the first element in an DataList_t.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

DataEntry_t* DataList_First( DataList_t* DL )

#else

DataEntry_t* DataList_First( DL )
DataList_t* DL;

#endif
{
    DataEntry_t* de;

    if( DL == NULL )
    {
        de = (DataEntry_t*) NULL;
    } else
    {
        de = DL->head->next;

        if( de == DL->tail )            /* List is empty */
            de = (DataEntry_t*) NULL;
    }
    return de;
}
