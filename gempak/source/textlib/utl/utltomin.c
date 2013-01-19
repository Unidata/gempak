#include "geminc.h"
#include "gemprm.h"

void utl_tomin ( float *anclat, float *anclon, float *newlat, 
                 float *newlon, int *iret )
/************************************************************************
 * utl_tomin								*
 *                                                                      *
 * This function converts lat/lon decimal to lat/lon minutes.    	*
 *                                                                      *
 * utl_tomin ( anclat, anclon, newlat, newlon, iret )                   *
 *                                                                      *
 * Input parameters:                                                    *
 *      *anclat		 float						*
 *	*anclon		 float						*
 *                                                                      *
 * Output parameters:                                                   *
 *	*newlat		 float						*
 *	*newlon		 float						*
 *      *iret            int            Return Code                     *
 *									*
 *					   -3 = Latitude not in range	*
 *					   -4 = Longitude not in range	*
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP	 5/03		Copied from VFTOMIN		*
 ***********************************************************************/
{
    int     lattmp, lontmp;
/*-------------------------------------------------------------------*/
    *iret  = 0;

   /*
    * Check for valid latitude and longitude values.
    */

    if ( ( *anclat< -90.0 ) || ( *anclat > 90.0 ) ) {
        *iret = -3;
	return;
    }

    if ( ( *anclon < -180.0 ) || ( *anclon > 180.0 ) ) {
	*iret = -4;
	return;
    }

   /*
    * Convert latitude from decimal to minutes.
    */

     lattmp = DDTODM ( G_ABS( *anclat ) );
    *newlat = (float)lattmp / 100.0F;

   /*
    * Convert longitude from decimal to minutes.
    */

     lontmp = DDTODM ( G_ABS( *anclon ) );
    *newlon = (float)lontmp / 100.0F;
}
