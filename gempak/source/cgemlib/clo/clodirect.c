#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

void clo_direct ( float *lat1, float *lon1, float *lat2, float *lon2, 
						float *dir, int *iret )
/************************************************************************
 * clo_direct                                                    	*
 *                                                                      *
 * This function figures out the direction from point 1 to point 2.	*
 *                                                                      *
 * clo_direct  ( lat1, lon1, lat2, lon2, dir, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*lat1	float	Latitude for point 1				*
 *	*lon1	float	Longitude for point 1				*
 *	*lat2	float	Latitude for point 2				*
 *	*lon2	float	Longitude for point 2				*
 *									*
 * Output parameters:                                                   *
 *	*dir	float	Direction of point 1 relative to point 2	*
 * 			(degrees from N)				*
 *	*iret	int	Return value                		        *
 *			=  0 - OK					*
 *			= -1 - input out of range      			*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * S. Law/GSC		01/99	moved from clo_tdirect			*
 * D.W.Plummer/NCEP	 8/99	make consistent w/ CLO_DLTLN, ie, apply	*
 *				spherical trig instead of cartesian trig*
 * D.W.Plummer/NCEP	 9/99	bug fix for when lon1 == lon2 (dlon=0)	*
 * D.W.Plummer/NCEP	 9/99	fix for when dlon > 180			*
 * M. Li/GSC		 10/99	Add a return value to check for invalid *
 *				lat/lon input; Modify to be called	*
 *				by FORTRAN				*
 * S. Jacobs/NCEP	12/99	Removed THETA macro, added error checks	*
 * M. Li/SAIC           11/01   Removed the check for small delta       *
 * D.W.Plummer/NCEP	12/01	Add limit check for acos expression	*
 ***********************************************************************/
{
    double	lat1d, lon1d, lat2d, lon2d;
    double	dlon, theta, alpha, val, tang;
/*---------------------------------------------------------------------*/

    *iret = 0;

/*
 *  Check the input latitude for out of range.
 */
    if  ( *lat1 < -90.0F || *lat1 > 90.0F || *lat2 < -90.0F 
				   	|| *lat2 > 90.0F ) {
        *iret = -1;
        *dir = RMISSD;
        return;
    }

/*
 *  Check the input longitude for out of range.
 */
    if  ( *lon1 < -180.0F || *lon1 > 180.0F || *lon2 < -180.0F 
                                          || *lon2 > 180.0F ) {
        *iret = -1;
        *dir = RMISSD;
        return;
    }

   
    lat1d = (double)(*lat1) * DTR;	
    lat2d = (double)(*lat2) * DTR;	

    if ( G_ABS((*lon1 - *lon2)) < 180.0F )  {
        lon1d = (double)(*lon1) * DTR;
        lon2d = (double)(*lon2) * DTR;
    }
    else  {
        lon1d = fmod( (double)(*lon1)+360.0, 360.0) * DTR;
        lon2d = fmod( (double)(*lon2)+360.0, 360.0) * DTR;
    }
    dlon = lon1d - lon2d;

    if ( G_DIFF(lat2d, HALFPI) )  {
        *dir = 180.0F;
    }
    else if ( G_DIFF(-lat2d, HALFPI) )  {
        *dir = 0.0F;
    }
    else  {

	val = (double)( sin(lat1d) * sin(lat2d) +
	        cos(lat1d) * cos(lat2d) * cos(dlon) );

	if  ( -1.0 <= val && val <= 1.0 )  {
	    theta = acos(val);

	    if ( G_DIFF(theta, 0.0) )  {
		*dir = 0.0F;
	    }
	    else  {
		tang = ( sin(lat1d) - sin(lat2d) * cos(theta) ) /
                        ( cos(lat2d) * sin(theta) );
		tang = G_MIN ( tang,  1.0 );
		tang = G_MAX ( tang, -1.0 );
		alpha = acos( tang );

		*dir = (float)(alpha*RTD);
		if ( dlon < 0.0L )  *dir = 360.0F - *dir;
	    }
	}
	else {
            *dir = 0.0F;
	}

    }

    return;
}
