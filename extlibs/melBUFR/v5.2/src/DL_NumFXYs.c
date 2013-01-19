/*
 * DataList_NumFXYs - VERSION: %I%  %E% %T%
 */
/*
 * DataList_NumFXYs() - Return the number of FXY values contained in
 * a DataList_t.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int DataList_NumFXYs( DataList_t* DL )

#else

int DataList_NumFXYs( DL )
DataList_t* DL;

#endif
{
    DataEntry_t* de;
    int          num_fxys;

    if( DL == NULL )
        return 0;

    de = DataList_First( DL );

    for( num_fxys=0; de != DL->tail; de=de->next )
    {
       num_fxys += de->num_fxys;
    }

    return num_fxys;
}
