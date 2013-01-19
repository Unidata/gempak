#include "dg.h"

void dg_clcn ( const int *nclust, const int *idofcl, const float *x,
               const float *y, int *idextr, int *iret ) 
/************************************************************************
 * dg_clcn								*
 *									*
 * This subroutine finds the center of gravity for a cluster, then  	*
 * finds the closest existing point on the grid. 			*
 *									*
 * dg_clcn ( nclust, idofcl, x, y, idextr, iret )			*
 *									*
 * Input parameters:							*
 *	*nclust		const int	Number of members in cluster	*
 *	*idofcl		const int	Indices of X and Y of cluster	*
 *	*X		const float	Column positions of extrema	*
 *	*Y		const float	Row positions of extrema	*
 *									*
 * Output parameters:							*
 *	*idextr		int		Index X and Y of cluster center	*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 **									*
 * Log:									*
 * D. Keiser/GSC	11/95						*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    float xt, yt, xcgrav, ycgrav, rmindis, dist;
    int iclust;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Find the sum of coordinates in each cluster.
     */
    xt = 0.;
    yt = 0.;
    for ( iclust = 0; iclust < (*nclust); iclust++ ) {
	xt += x[idofcl[iclust]-1];
	yt += y[idofcl[iclust]-1];
    }

    /*
     * Find the mean of the coordinates in each cluster.
     */
    xcgrav = xt / (*nclust); 
    ycgrav = yt / (*nclust); 

    /*
     * Find the point on the grid that is the least distance from the
     * center of gravity.
     */
    rmindis = 99999.0;
    for ( iclust = 0; iclust < (*nclust); iclust++ ) {
	dist = ( x[idofcl[iclust]-1] - xcgrav ) *
	       ( x[idofcl[iclust]-1] - xcgrav ) +
	       ( y[idofcl[iclust]-1] - ycgrav ) *
	       ( y[idofcl[iclust]-1] - ycgrav );

	if ( dist < rmindis ) {
	    rmindis = dist;
	    *idextr = idofcl[iclust];
	}
    }

    return;
}
