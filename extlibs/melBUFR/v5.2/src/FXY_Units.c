/*
 * FXY_Units - VERSION: %I%  %E% %T%
 */
/*
 * FXY_Units - Return units for given FXY value.
 * 
 * CHANGE LOG
 * 
 * 022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

char* FXY_Units( FXY_t FXY_Val )

#else

char* FXY_Units( FXY_Val )
FXY_t FXY_Val;

#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;
    Descriptor_t* descriptor;

    char buf[80];

    if( (descriptor=TableB_Get( FXY_Val )) == NULL )
    {
        sprintf( buf, "Non-Table B descriptor (%s)", FXY_String( FXY_Val ) );
        BUFR_Err_Set( "FXY_Units", buf );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_Units", buf);
        return NULL;
    }
    else
        return descriptor->units;
}
