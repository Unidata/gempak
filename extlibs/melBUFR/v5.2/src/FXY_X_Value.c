/*
 * FXY_X_Value - VERSION: %I%  %E% %T%
 */
/*
 * FXY_X_Value - Return the X portion of an FXY value.
 */
/*
 *
 CHANGE LOG
 *
 * 100997 LAH: Added int cast
 *
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int FXY_X_Value( FXY_t fxy )

#else

int FXY_X_Value( fxy )
FXY_t fxy;

#endif
{
    /* 100997 LAH: Added int cast */
    return ( (int)fxy >> NUM_Y_BITS) & MAX_X_VAL;
}
