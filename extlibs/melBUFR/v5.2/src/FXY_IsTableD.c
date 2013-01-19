/*
 * FXY_IsTableD - VERSION: %I%  %E% %T%
 */
/*
 * FXY_IsTableD - Return 1 if given descriptor is a valid Table D descriptor,
 * otherwise return 0.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int FXY_IsTableD( FXY_t FXYval )

#else

int FXY_IsTableD( FXYval )
FXY_t FXYval;

#endif
{
    if( FXYval > (FXY_t)MAX_FXY_VAL )
        return 0;
    else
        return FXY_F_Value( FXYval ) == TABLE_D_F_VAL;
}
