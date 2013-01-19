/*
 * FXY_Unpack - VERSION: %I%  %E% %T%
 */
/*
 * FXY_Unpack - Unpack F, X, and Y values from an FXY_t.
 * Return 1 on error, otherwise 0.
 */
/*
 * CHANGE LOG
 *
 * 100997 LAH:  Added uint_t casts 
 * 022498 LAH:  Added prints to bufr_log file.
 *
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int FXY_Unpack( FXY_t val, uint_t *F, uint_t *X, uint_t *Y )

#else

int FXY_Unpack( val, F, X, Y )
FXY_t  val;
uint_t *F, *X, *Y;

#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;
    if( F == NULL )

    {
        BUFR_Err_Set( "FXY_Unpack", "NULL F value pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_Unpack", 
	     "NULL F value pointer" );
        return 1;
    }

    if( X == NULL )
    {
        BUFR_Err_Set( "FXY_Unpack", "NULL X value pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_Unpack", 
	     "NULL X value pointer" );
        return 1;
    }

    if( Y == NULL )
    {
        BUFR_Err_Set( "FXY_Unpack", "NULL Y value pointer" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_Unpack", 
	     "NULL Y value pointer" );
        return 1;
    }

    /* 100997 LAH:  Added uint_t casts */
    *F = (uint_t) FXY_F_Value( val );
    *X = (uint_t) FXY_X_Value( val );
    *Y = (uint_t) FXY_Y_Value( val );

    return 0;
}
