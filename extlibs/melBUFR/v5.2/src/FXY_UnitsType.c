/*
 * FXY_UnitsType - VERSION: %I%  %E% %T%
 */
/*
 * FXY_UnitsType - Return Units for given FXY value.
 * 
 * CHANGE LOG
 * 
 * 022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

Units_t FXY_UnitsType( FXY_t FXY_Val )

#else

Units_t FXY_UnitsType( FXY_Val )
FXY_t FXY_Val;

#endif
{
    extern BUFR_Cntl_t BUFR_Cntl;
    Descriptor_t* descriptor;

    char buf[80];

    if( (descriptor=TableB_Get( FXY_Val )) == NULL )
    {
        sprintf( buf, "Non-Table B descriptor (%s)", FXY_String( FXY_Val ) );
        BUFR_Err_Set( "FXY_UnitsType", buf );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "FXY_UnitsType", buf);
        return UNKNOWN_UNIT;
    }
    else
        return descriptor->units_type;
}
