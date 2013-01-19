#include "dg.h"

void dg_fclp ( const int *krad, const int *icntpt, const int *mcnt,
               const float *x, const float *y, int *keep, int *idofcl,
	       int *nclust, int *iret )
/************************************************************************
 * dg_fclp								*
 *									*
 * This subroutine determines whether or not a cluster exists.		*
 *									*
 * dg_fclp ( krad, icntpt, mcnt, x, y, keep, idofcl, nclust, iret )	*
 *									*
 * Input parameters:							*
 *	*krad		const int	Search radius (grid index units)*
 *	*icntpt		const int	Pointer in X and Y arrays	*
 *	*mcnt		const int	Total # of relative extrema 	*
 *	*x		const float	Column positions of extrema	*
 *	*y		const float	Row positions of extrema	*
 *									*
 * Input and Output parameters:						*
 *	*keep		int		Storage for internal flags	*
 *									*
 * Output parameters:							*
 *	*idofcl		int		Indices of X and Y of cluster 	*
 *	*nclust		int		Number of members in cluster	*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 **									*
 * Log:									*
 * D. Keiser/GSC	11/95						*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    int iptrcl, iptcnt;
/*----------------------------------------------------------------------*/
    *iret = 0;
    *nclust = 1;

    /*
     * Initialize cluster array with one point as the only member.
     */
    idofcl[(*nclust)-1] = *icntpt;
    iptrcl = 1;

    /*
     * Loop through all points in cluster.
     */
    while ( iptrcl <= (*nclust) ) {
	/*
	 * Loop through all extrema.
	 */
	for ( iptcnt = 1; iptcnt <= (*mcnt); iptcnt++ ) {
	    /*
	     * Check only points that are still present.
	     */
	    if ( keep[iptcnt-1] == G_TRUE ) {
		/*
		 * If two points are within KRAD of each other, then they
		 * belong to a cluster.
		 */
		if ( ( G_ABS ( x[idofcl[iptrcl-1]-1] - x[iptcnt-1] ) <= (*krad) ) &&
		     ( G_ABS ( y[idofcl[iptrcl-1]-1] - y[iptcnt-1] ) <= (*krad) ) ) {
		    /*
		     * Another member of cluster found.
		     */
		    (*nclust)++;
		    idofcl[(*nclust)-1] = iptcnt;

		    /*
		     * Set flag to remove this point.
		     */
		    keep[iptcnt-1] = G_FALSE;
		}
	    }
	}
	iptrcl++;
    }

    return;
}
