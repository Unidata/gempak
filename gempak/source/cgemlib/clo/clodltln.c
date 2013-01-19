#include "geminc.h"
#include "gemprm.h"

void clo_dltln ( float *alat, float *alon, float *dist, float *dir, 
				float *blat, float *blon, int *iret )
/************************************************************************
 * clo_dltln                                                    	*
 *                                                                      *
 * This function takes a latitude/longitude point, a distance and a	*
 * direction and computes the resulting latitude/longitude point.	*
 *                                                                      *
 * clo_dltln ( alat, alon, dist, dir, blat, blon, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *	*alat		float		Latitude			*
 *	*alon		float		Longitude			*
 *	*dist		float		Distance (meters)		*
 *	*dir		float		Direction (degrees from N)	*
 *									*
 * Output parameters:                                                   *
 *	*blat		float		Output latitude			*
 *	*blon		float		Output longitude		*
 *	*iret		int		Return value			*
 *					  -1 = input out of range	*
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NCEP	 5/98						*
 * S. Jacobs/NCEP	 5/98	Fixed for poles and dateline		*
 * T. Piper/GSC		09/99	Fixed to use spherical coordinates	*
 * M. Li/GSC		10/99	Added a check for invalid input;	*	
 *				Modified to be called by FORTRAN	*
 ***********************************************************************/
{

	double	clat, clon, cdist, cdir, dlat, dlon, dlt, dln;

/*---------------------------------------------------------------------*/

	*iret = 0;

/*
 *	Check the input latitude for out of range.
 */
	if  ( *alat < -90.0F || *alat > 90.0F )  {
	    *iret = -1;
	    *blat = RMISSD;
	    *blon = RMISSD;
	    return;
	}

/*
 *	Check the input longitude for out of range.
 */
	if  ( *alon < -180.0F || *alon > 180.0F )  {
	    *iret = -1;
	    *blat = RMISSD;
	    *blon = RMISSD;
	    return;
	}


/*
 *	Convert the input values to radians.
 */
	cdir = (double)(*dir) * DTR;
	clat = (double)(*alat) * DTR;
	clon = (double)(*alon) * DTR;
	cdist = (double)(*dist / RADIUS);
	
	dlat = asin(sin(clat)*cos(cdist)+cos(clat)*sin(cdist)*cos(cdir));

/*
 *	Make sure the longitude is between -180 and +180 degrees.
 */
	clon = clon - (double)( (double)((int)(clon/TWOPI)) * TWOPI  );
	if  ( clon < -PI )  clon = clon + TWOPI;
	if  ( clon >  PI )  clon = clon - TWOPI;

/*
 *	Compute the delta longitude.  If the initial latitude is either
 *	pole, then use the original longitude, otherwise, compute
 *	the new longitude.
 */
	if  ( G_DIFF(*alat, 90.0F) || G_DIFF(-*alat, 90.0F) )  {
	    dlon = clon;
	}
	else {
	    dlon = atan2( sin(cdir) * sin(cdist) * cos(clat), \
				     cos(cdist)-sin(clat)*sin(dlat));
	    dlon = fmod(clon+dlon + PI, TWOPI) - PI;	
	}

/*
 *	Make sure that latitude is between -90 and +90 degrees.
 *	Adjust the longitude, if necessary.
 */
	dlt = dlat - (double)( (double)((int)(dlat/PI)) * PI );

	if  ( dlt >  HALFPI )  {
	    dlt  =  PI - dlt;
	    dlon = -dlon;
	}
	if  ( dlt < -HALFPI )  {
	    dlt  = -PI - dlt;
	    dlon = -dlon;
	}

/*
 *	Make sure the longitude is between -180 and +180 degrees.
 */
	dln = dlon - (double)( (double)((int)(dlon/TWOPI)) * TWOPI );
	if  ( dln < -PI )  dln = dln + TWOPI;
	if  ( dln >  PI )  dln = dln - TWOPI;

/*
 *	Convert the new position to degrees.
 */
	*blat = (float)(dlt * RTD);
	*blon = (float)(dln * RTD);

	return;

}
