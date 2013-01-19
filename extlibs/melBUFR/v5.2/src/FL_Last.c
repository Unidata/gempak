/*
 * FXY_List_Last - VERSION: %I%  %E% %T%
 */
/*
 * FXY_List_Last() - Return a pointer to the last element in an FXY_List_t.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

FXY_Entry_t* FXY_List_Last( FXY_List_t* FL )

#else

FXY_Entry_t* FXY_List_Last( FL )
FXY_List_t* FL;

#endif
{
/*   register FXY_Entry_t* fe; */

    if( FL == NULL )
        return (FXY_Entry_t*) NULL;

    /* 121697 LAH: Replaced following code with return utilizing */
    /*             new element added to FXY_List_t structure     */
    /*             that is used to count entries as they are made */
/*
    for( fe=FL->head; fe->next != FL->tail; fe=fe->next )
        ;

    return fe;
*/
    return(FL->last);
}
