/*
 * FXY_PrintVals - VERSION: %I%  %E% %T%
 */
/*
 * FXY_PrintVals - Debug function to print the contents of an FXY_t array.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

void FXY_PrintVals( FXY_t* FXY_Vals, int NumFXYs, FILE* fp )

#else

void FXY_PrintVals( FXY_Vals, NumFXYs, fp )
FXY_t* FXY_Vals;
int    NumFXYs;
FILE*  fp;

#endif
{
    FXY_t* fxy;
    int    i, n;

    if( FXY_Vals == NULL )
        return;

    if( NumFXYs < 1 )
        return;

    if( fp == NULL )
        fp = stdout;

    for( i=0, n=0, fxy=FXY_Vals; i < NumFXYs; i++, fxy++ )
    {
        FXY_Print( *fxy, fp );
        n += 8;

        if( i != (NumFXYs-1) )
        {
            fprintf( fp, ", " );
            n += 2;
        }

        if( n > 72 )
        {
            fprintf( fp, "\n" );
            n = 0;
        }
    }

    fprintf( fp, "\n" );

    fflush( fp );
}
