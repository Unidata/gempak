/*
 * FXY_Pack - VERSION: %I%  %E% %T%
 */
/*
 * FXY_Pack - Return packed values of F, X, and Y.
 * Return BAD_FXY_VAL on error.
 */
/*
 * CHANGE LOG
 *
 * 100897 LAH: Added FXY_t (unsigned long) cast
 *
 */
 
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

FXY_t FXY_Pack( int F, int X, int Y )

#else

FXY_t FXY_Pack( F, X, Y )
int F, X, Y;

#endif
{
    FXY_t val;

    if( F < 0 || F > MAX_F_VAL ) return (FXY_t) BAD_FXY_VAL;
    if( X < 0 || X > MAX_X_VAL ) return (FXY_t) BAD_FXY_VAL;
    if( Y < 0 || Y > MAX_Y_VAL ) return (FXY_t) BAD_FXY_VAL;

    /* 100897 LAH: Added FXY_t (unsigned long) cast */
    /* 102097 LAH: Added u_long casts */
    val = (FXY_t)(F << (NUM_X_BITS+NUM_Y_BITS)) | 
          ( (ulong_t)X << NUM_Y_BITS) | (ulong_t) Y;

    return val;
}
