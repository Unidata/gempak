#include "geminc.h"
#include "gemprm.h"

void cgr_reorder ( int *npts, float *xin, float *yin, int *indx,
		   int *iret ) 
/************************************************************************
 * cgr_reorder								*
 *                                                                      *
 * This function reorders a closed polygon into a counter-clockwise	*
 * fashion with the first point having the greatest x-value.		*
 * A cartesian coordinate system is assumed.				*
 *                                                                      *
 * cgr_reorder ( npts, xin, yin, indx, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*npts		int		Number of points		*
 *	*xin		float		X-coordinates			*
 *	*yin		float		Y-coordinates			*
 *									*
 * Output parameters:                                                   *
 *      *indx         	int             indexes of points		*
 *	*iret		int		Return value			*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	11/03						*
 ***********************************************************************/
{
int	ii, jj, iptr, np, inc, ier;
float	maxxin, dirnext, dirprev;
/*---------------------------------------------------------------------*/

    *iret = 0;

    np = *npts;
        for ( jj = 0; jj < np; jj++ ) {
        indx[jj] = jj;
    }

    /*
     *  Re-order points to start with the one with the
     *  the greatest x-value and proceed counter-clockwise.
     */

    maxxin = -FLT_MAX;
    for ( jj = 0; jj < np; jj++ )  {
        if ( xin[jj] >= maxxin  ||
		( G_DIFF(xin[jj], maxxin) && yin[jj] < yin[iptr] ) )  {
            iptr = jj;
            maxxin = xin[jj];
        }
    }

    /*
     *  Check directions for each adjacent point; the point with
     *  the smallest angle counter-clockwise direction.
     */
    cgr_dang ( &xin[iptr], &yin[iptr], 
	       &xin[(iptr-1+np)%np], &yin[(iptr-1+np)%np], &dirprev, &ier );
    dirprev = G_MOD ( (dirprev+360.0F), 360.0F );
    cgr_dang ( &xin[iptr], &yin[iptr], 
	       &xin[(iptr+1+np)%np], &yin[(iptr+1+np)%np], &dirnext, &ier );
    dirnext = G_MOD ( (dirnext+360.0F), 360.0F );

    inc = +1;
    if ( dirnext > dirprev )  inc = -1;

    ii = 0;
    jj = iptr;
    indx[ii] = jj;
	ii++;
    jj  = (jj+inc+np) % np;
    while ( jj != iptr )  {
	indx[ii] = jj;
	ii++;
	jj  = (jj+inc+np) % np;
    }

    return;

}
