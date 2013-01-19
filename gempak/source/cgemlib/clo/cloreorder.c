#include "geminc.h"
#include "gemprm.h"

void clo_reorder ( int npin, float *lat, float *lon, int *indx,
						int *iret ) 
/************************************************************************
 * clo_reorder								*
 *                                                                      *
 * This function reorder a closed polygon into a clockwise fashion	*
 * and the first point is the northernmost point.			*
 *                                                                      *
 * clo_reorder ( npin, lat, lon, indx, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	npin		int		Number of points		*
 *	*lat		float		Latitudes			*
 *	*lon		float		Longitudes			*
 *									*
 * Output parameters:                                                   *
 *       indx         	int             indexes of points		*
 *	*iret		int		Return value			*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		08/01	Create					*
 * m.gamazaychikov/SAIC	 9/02	change output parameters from arrays of *
 *				lat and lon to array of indexes		*
 * S. Jacobs/NCEP	10/02	Removed increase of np			*
 * E. Safford/SAIC	11/06	initialize iptr    			*
 ***********************************************************************/
{
int	ii, jj, iptr=0, np, ier;
float	maxlat, dirnext, dirprev;
/*---------------------------------------------------------------------*/

	*iret = 0;

	np = npin;
	for ( jj = 0; jj < np; jj++ ) {
	    indx[jj] = jj;
	}

     	/*
	 *  Re-order processing of points to do northernmost first 
	 *  and proceed clockwise.
	 */

	maxlat = -90.0F;
	for ( jj = 0; jj < np; jj++ )  {
	    if ( lat[jj] > maxlat )  {
	        iptr = jj;
	        maxlat = lat[jj];
	    }
  	}

        /*
         *  Check directions for each adjacent point; the point with
         *  the smallest angle from north is in the clockwise direction.
         */
        if ( iptr != 0 )
	    clo_direct ( &lat[iptr-1], &lon[iptr-1], 
	    		 &lat[iptr], &lon[iptr], &dirprev, &ier );
        else
	    clo_direct ( &lat[np-1], &lon[np-1], 
	    		 &lat[iptr], &lon[iptr], &dirprev, &ier );

        if ( iptr != np-1 )
            clo_direct ( &lat[iptr+1], &lon[iptr+1], 
	    		 &lat[iptr], &lon[iptr], &dirnext, &ier );
        else
            clo_direct ( &lat[0], &lon[0], 
	    		 &lat[iptr], &lon[iptr], &dirnext, &ier );

        if ( dirnext < dirprev )  {
	    ii = 0;
	    for ( jj = iptr; jj < np; jj++ )  {
	    	indx[ii] = jj;
	    	ii++;
	    }
	    for ( jj = 0; jj <= iptr; jj++ )  {
	    	indx[ii] = jj;
	    	ii++;
	    }
        }
        else  {
	    ii = 0;
	    for ( jj = iptr; jj >= 0; jj-- )  {
	    	indx[ii] = jj;
	    	ii++;
	    }
	    for ( jj = np-1; jj >= iptr; jj-- )  {
	    	indx[ii] = jj;
	    	ii++;
	    }
        }

	return;

}
