/*
 * FXY_Get_Scale - VERSION: %I%  %E% %T%
 */
/*
 * FXY_Get_Scale - Return scale for given FXY value or 0 on error.
 * 
 * CHANGE LOG
 * 
 * 022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int FXY_Get_Scale( FXY_t FXY_Val )

#else

int FXY_Get_Scale( FXY_Val )
FXY_t FXY_Val;

#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;
    Descriptor_t* descriptor;

    char buf[80];

    if( (descriptor=TableB_Get( FXY_Val )) == NULL )
    {
        sprintf( buf, "Non-Table B descriptor (%s)", FXY_String( FXY_Val ) );
        BUFR_Err_Set( "FXY_Get_Scale", buf );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_Get_Scale", buf);
        return 0;
    } else
        return descriptor->scale;
}
