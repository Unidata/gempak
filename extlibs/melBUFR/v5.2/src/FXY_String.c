/*
 * FXY_String - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

char* FXY_String( FXY_t FXY_Val )

#else

char* FXY_String( FXY_Val )
FXY_t FXY_Val;

#endif
{
    static char str[9];
    uint_t f, x, y;

    FXY_Unpack( FXY_Val, &f, &x, &y );

    sprintf( str, "%d-%02d-%03d", f, x, y );

    return str;
}
