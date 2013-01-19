#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

extern  SpcInfo_t   newinfo;
extern  SpcInfo_t   spcinfo;

void vftomin ( float anclat, float anclon, float *newlat, 
					float *newlon, int *iret )
/************************************************************************
 * vftomin								*
 *                                                                      *
 * This function converts the decimal places for lat/lon to minutes.    *
 *                                                                      *
 * vftomin ( anclat, anclon, newlat, newlon, iret )                     *
 *                                                                      *
 * Input parameters:                                                    *
 *      anclat		float						*
 *	anclon		float						*
 *	*newlat		float						*
 *	*newlon		float						*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret            int            Return Code                     *
 *									*
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC          8/99   Created                                 *
 * A. Hardy/GSC          9/99   Corrected prolog description		*
 * A. Hardy/GSC         11/99   Corrected prolog function name		*
 * A. Hardy/GSC		 2/00   Extracted from SPCTXT                   *
 ***********************************************************************/
{
    float   minchg, latmins, lonmins;
    int     latpts, lonpts;
/*-------------------------------------------------------------------*/
    *iret  = 0;
    minchg = .60;

    latpts  = (anclat * 100) ;
    latmins = ( (latpts % 100) * minchg ) * .01  ;
    latpts  = anclat * 1;
    *newlat = (float) latpts + latmins;

    lonpts  = (anclon * 100) ;
    lonmins = ( (lonpts % 100) * minchg ) * .01  ;
    lonpts  = anclon * 1;
    *newlon = (float) lonpts + lonmins;
}
