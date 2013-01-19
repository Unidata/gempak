/*
 * FXY_List_Size - VERSION: %I%  %E% %T%
 */
/*
 * FXY_List_Size() - Return the number of elements in an FXY_List_t.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int FXY_List_Size( FXY_List_t* FL )

#else

int FXY_List_Size( FL )
FXY_List_t* FL;

#endif
{
/*
    FXY_Entry_t* fe;
    int          size;
*/

    if( FL == NULL )
        return 0;
/*
 * 12/15/97 LAH:  Replaced following two lines of code.  The new FXY_List
 *                structure includes a counter that is updated whenever
 *                the FXY_List is modified
 */
/*
    for( fe=FL->head, size=0; fe->next != FL->tail; fe=fe->next )
        size++;
*/
    return FL->num_fxys;
}
