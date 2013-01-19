/*
 * BUFR_Val_Array_free - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Val_Array_free - Free memory allocated to each BUFR_Val_t in the given array
 * and reset the values.
 *
 * NOTE: This function does not free memory allocated for the array itself.
 * To do so would invite disaster!
 *
 * CAUTION: If any of the BUFR_Val_t's have uninitialized or bad pointers, the
 * program may crash.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void BUFR_Val_Array_free( BUFR_Val_t* ValArray, int NumVals )

#else

void BUFR_Val_Array_free( ValArray, NumVals )
BUFR_Val_t* ValArray;
int         NumVals;

#endif
{
    BUFR_Val_t* bv;
    int         i;

    if( ValArray == NULL )
        return;

    if( NumVals < 1 )
        return;

    for( i=0, bv=ValArray; i < NumVals; i++, bv++ )
    {
       BUFR_Val_free(bv);
/*
        free( (void*) bv->AF );
        free( (void*) bv->AF_sig );
        bv->num_AFs = 0;

        bv->FXY_Val = (FXY_t) BAD_FXY_VAL;

        if( bv->Val_Type == DT_STRING )
            free( (void*) &bv->Val.string );
        else
            bv->Val.number = 0.0;

        bv->Val_Type = DT_UNKNOWN;
*/
    }
}
