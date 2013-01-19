/*
 * FXY_Print - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void FXY_Print( FXY_t FXY_Val, FILE* fp )

#else

void FXY_Print( FXY_Val, fp )
FXY_t FXY_Val;
FILE* fp;

#endif
{
    if( fp == NULL )
        fp = stdout;

    fprintf( fp, "%s", FXY_String( FXY_Val ) );
}
