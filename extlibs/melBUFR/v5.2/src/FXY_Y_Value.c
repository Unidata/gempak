/*
 * FXY_Y_Value - VERSION: %I%  %E% %T%
 */
/*
 * FXY_Y_Value - Return the Y portion of an FXY value.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int FXY_Y_Value( FXY_t fxy )

#else

int FXY_Y_Value( fxy )
FXY_t fxy;

#endif
{
    /* 100997 LAH: Added int cast */
    return ((int)fxy & MAX_Y_VAL);
}
