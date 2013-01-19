/*
 * FXY_IsTableC - VERSION: %I%  %E% %T%
 */
/*
 * FXY_IsTableC - Return 1 if given descriptor is a valid Table C descriptor,
 * otherwise return 0.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int FXY_IsTableC( FXY_t FXYval )

#else

int FXY_IsTableC( FXYval )
FXY_t FXYval;

#endif
{
    if( FXYval > (FXY_t)MAX_FXY_VAL )
        return 0;
    else
        return FXY_F_Value( FXYval ) == TABLE_C_F_VAL;
}
