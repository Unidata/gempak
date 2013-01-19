/*
 * FXY_IsTableB - VERSION: %I%  %E% %T%
 */
/*
 * FXY_IsTableB - Return 1 if given descriptor is a valid Table B descriptor,
 * otherwise return 0.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int FXY_IsTableB( FXY_t FXYval )

#else

int FXY_IsTableB( FXYval )
FXY_t FXYval;

#endif
{
    if( FXYval > (FXY_t)MAX_FXY_VAL )
        return 0;
    else
        return FXY_F_Value( FXYval ) == TABLE_B_F_VAL;
}
