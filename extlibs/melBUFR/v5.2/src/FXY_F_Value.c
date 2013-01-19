/*
 * FXY_F_Value - VERSION: %I%  %E% %T%
 */
/*
 * FXY_F_Value - Return the F portion of an FXY value.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int FXY_F_Value( FXY_t fxy )

#else

int FXY_F_Value( fxy )
FXY_t fxy;

#endif
{
    /* 102097 LAH: Added int cast */
    return (int)((fxy >> (NUM_X_BITS+NUM_Y_BITS)) & MAX_F_VAL);
}
