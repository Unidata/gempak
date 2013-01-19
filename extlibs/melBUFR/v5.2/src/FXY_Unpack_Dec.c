/*
 * FXY_Unpack_Dec - VERSION: %I%  %E% %T%
 */
/*
 * FXY_Unpack_Dec - Given an FXY_t value, convert it to a decimal FXY value.
 * For example, given 49409 (3-01-001), return 301001.
 * Return BAD_VAL on error.
 */
/*
 * CHANGE LOG
 * 
 * 100997 LAH: Added int cast
 *
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int FXY_Unpack_Dec( FXY_t PackedFXY )

#else

int FXY_Unpack_Dec( PackedFXY )
FXY_t  PackedFXY;

#endif
{
    uint_t f, x, y;
    int    DecimalFXY;

    if( FXY_Unpack( PackedFXY, &f, &x, &y ) )
    {
        BUFR_Err_Log( "FXY_Unpack_Dec" );
        return BAD_VAL;
    }

    /* 100997 LAH: Added int cast */
    DecimalFXY = (int) (f*100000 + x*1000 + y);

    return DecimalFXY;
}
