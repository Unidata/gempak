#include "dg.h"

void dg_clmp ( const int *krad, int *mcnt, float *x, float *y, float *v,
               int *idofcl, int *keep, int *iret ) 
/************************************************************************
 * dg_clmp								*
 *									*
 * This subroutine locates a cluster of equal relative extrema and  	*
 * selects one point out of that group. The other extrema in the 	*
 * cluster are discarded.						*
 *									*
 * dg_clmp ( krad, mcnt, x, y, v, idofcl, keep, iret )			*
 *									*
 * Input parameters:							*
 *	*krad		const int	Search radius (grid index units)*
 *									*
 * Input and Output parameters:						*
 *	*mcnt		int		The number of relative extrema	*
 *	*x		float		Column positions of extrema	*
 *	*y		float		Row positions of extrema	*
 *	*v		float		Extrema				*
 *									*
 * Output parameters:							*
 *	*idofcl		int		Storage for indices into v   	*
 *	*keep		int		Storage for internal flags 	*
 *	*iret		int		Return code			*
 *					   0 = normal return		*
 **									*
 * Log:									*
 * D. Keiser/GSC	11/95						*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    int nclust, icntpt, idextr, i;
/*----------------------------------------------------------------------*/
    *iret = 0;

    /*
     * Initalize all extrema present.
     */
    for ( i = 0; i < (*mcnt); i++ ) {
	keep[i] = G_TRUE;
    }

    /*
     * Loop through all extrema.
     */
    for ( icntpt = 1; icntpt <= (*mcnt); icntpt++ ) {
	/*
	 * If extremum is present, process it. 
	 */
	if ( keep[icntpt-1] == G_TRUE ) {
	    keep[icntpt-1] = G_FALSE;

	    /*
	     * Find next cluster.
	     */
	    dg_fclp ( krad, &icntpt, mcnt, x, y, keep, idofcl, &nclust, iret );

	    /*
	     * If there is a cluster, find it's center.
	     */
	    if ( nclust > 1 ) {
		dg_clcn ( &nclust, idofcl, x, y, &idextr, iret );

		/*
		 * Put center of cluster back.
		 */
		keep[idextr-1] = G_TRUE;
	    } else {
		/*
		 * *Single point is not cluster, put it back.
		 */
		keep[icntpt-1] = G_TRUE;
	    }
	}
    }

    /*
     * Collapse arrays by keeping some points, removing rest.
     */
    dg_cola ( mcnt, x, y, v, keep, iret );

    return;
}
