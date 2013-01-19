#include "geminc.h"
#include "gemprm.h"

void clo_lonin ( float *lon, int nlon, float lon1, float lon2, int maxhot, 
					int *hotlist, int *nhot, int *iret )
/************************************************************************
 * clo_lonin								*
 *									*
 * This function extracts an array of integers which are offsets into	*
 * the longitude array.  They equate to the longitude array entries	*
 * which are within the area bounded by the parameters of this call.	*
 * Assume the longitude array is pre-sorted from east to west.		*
 *									*
 * clo_lonin ( lon, nlon, lon1, lon2, maxhot, hotlist, nhot, iret )	*
 *									*
 * Input parameters:							*
 *	*lon		float	Longitude array				*
 *	nlon		int	Number of longitudes			*
 *	lon1		float	Longitude bound #1			*
 *	lon2		float	Longitude bound #2			*
 *	maxhot		int	Maximum hotlist entries allowed 	*
 *									*
 * Output parameters:							*
 *	*hotlist	int	Indices of longitudes in bounds		*
 *	*nhot		int	# of longitudes that fell within bounds	*
 *	*iret		int	Return code				*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 6/97	Created					*
 * T. Piper/GSC		10/98	Prolog update				*
 * D.W.Plummer/NCEP	 1/99	Bug fix for upper_bnd calculation	*
 * D.W.Plummer/NCEP	 1/99	Bug fix for _bnd calculations		*
 * B .Yin/SAIC		 7/04	Bug fix for out of array bound		*
 ***********************************************************************/
{
float	ll_lon, ur_lon;
int	lower_bnds, upper_bnds, i, hi, lo, mid;
/*---------------------------------------------------------------------*/
    *iret = 0;
    *nhot = 0;

    /*
     *  Figure minimum and maximum longitudes.
     */
    ll_lon = G_MIN ( lon1, lon2 );
    ur_lon = G_MAX ( lon1, lon2 );

    /* 
     *  Set the upper and lower bound limits in the longitude array
     *  by looking for the bounding longitudes.  Assume the longitude
     *  array has been pre-sorted east to west.  Use binary search.
     */
    i = -1;
    lo = 0;
    hi = nlon-1;
    while ( lo <= hi )  {
	mid = (lo + hi) / 2;
        if ( (lon[mid] > ur_lon) && (lon[mid+1] <= ur_lon) )  {
	    break;
	}
	else  {
	    if ( lon[mid] > ur_lon )
		lo = mid + 1;
	    else
		hi = mid - 1;
	}
    }
    i = mid;
    upper_bnds = i + 1;
    if ( upper_bnds == 1 && lon[0] <= ur_lon )  upper_bnds = 0;

    i = -1;
    lo = 0;
    hi = nlon-1;
    while ( lo <= hi )  {
	mid = (lo + hi) / 2;
        if ( ( mid == hi ) || ((lon[mid] > ll_lon) && (lon[mid+1] <= ll_lon)) )  {
	    break;
	}
	else  {
	    if ( lon[mid] > ll_lon )
		lo = mid + 1;
	    else
		hi = mid - 1;
	}
    }
    i = mid;
    lower_bnds = i + 1;
    if ( lower_bnds == 1 && lon[0] <= ur_lon )  lower_bnds = 0;

    /* 
     *  Put every location between the bounds into the hotlist.
     */

    for ( i = upper_bnds; i < lower_bnds; i++ )  {
	if ( *nhot < maxhot )  {
	    hotlist[*nhot] = i;
	    (*nhot)++;
    	}
    }

}
