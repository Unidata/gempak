#include "dg.h"

#define MXM		256

void dg_hilo ( const float *grid, const int *kx, const int *ky,
               const int *ksrad, const int *intflg, const int *nmax,
	       const int *nmin, const float *rlomax, const float *rhimax,
	       const float *rlomin, const float *rhimin, int * nummax, 
	       float *xmax, float *ymax, float *vmax, int *nummin,
	       float *xmin, float *ymin, float *vmin, int *iret )
/************************************************************************
 * dg_hilo								*
 *									*
 * This subroutine locates relative minima and maxima over a grid.	*
 * Up to NMAX maxima and NMIN minima are found.  Minima and maxima may 	*
 * be restricted to a specified range of values.  If the range bounds	*
 * are equal (for example 0 and 0), all extrema are found up to the 	*
 * maximum number requested.  If the INTFLG is true, interpolations are *
 * done to estimate the extrema at off-grid point positions.  Radius	*
 * KSRAD is a scaling factor with a default of 3.  It defines a moving	*
 * search area where extrema are found.					*
 *									*
 * dg_hilo ( grid,  kx,  ky,  ksrad,  intflg,  nmax,  nmin, rlomax,	*
 *	     rhimax,  rlomin,  rhimin, nummax,  xmax,  ymax,  vmax,	*
 *	     nummin,  xmin,  ymin, vmin, iret  )			* 
 *									*
 * Input parameters:							*
 *	grid		const float	Array of grid point values	*
 *	kx		const int	Number of grid columns		*
 *	ky		const int	Number of grid rows		*
 *	ksrad		const int	Search radius (grid index units)*
 *	intflg		const int	Flag to interpolate		*
 *	nmax		const int	Maximum number of highs		*
 *	nmin		const int	Maximum number of lows		*
 *	rlomax		const float	Lower bound on maxima		*
 *	rhimax		const float	Upper bound on maxima		*
 *	rlomin		const float	Lower bound on minima		*
 *	rhimin		const float	Upper bound on minima		*
 *									*
 * Output parameters:							*
 *	nummax		int		Number of maxima found		*
 *	xmax		float		Column positions of maxima	*
 *	ymax		float		Row positions of maxima		*
 *	vmax		float		Values of maxima		*
 *	nummin		int		Number of minima found		*
 *	xmin		float		Column positions of minima	*
 *	ymin		float		Row positions of minima		*
 *	vmin		float		Values of minima		*
 *	iret		int		Return code			*
 *					  0 = normal return		*
 *					 -1 = grid size is too large	*
 *					  1 = too many mins found	*
 *					  2 = too many maxs found	*
 *					  3 = too many mins and maxs	* 
 **									*
 * Log:									*
 * K. Brill/NMC		05/93						*
 * D. Keiser/GSC	07/95	Renamed to DG_HILO 			*
 * D. Keiser/GSC	11/95	Revamped sorting, added cluster 	*
 *					and clipping logic, cleaned up	*
 * M. Linda/GSC		 4/96	Removed IFIX so that AIX compiles	*
 * R. Tian/SAIC		 2/06	Recoded from Fortran			*
 ************************************************************************/
{
    float xextr[MXM], yextr[MXM], vextr[MXM], rmin, rmax, qxm1, qxp1,
          qym1, qyp1, xq, yq, vq, dqdx, dqdy, dqdx2, dqdy2, dx, dy,
	  temp1, temp2, temp3;
    int keep[MXM], sorted, dopole, alleq, ok, polen, poles, wrapc, wrapa;
    int idofcl[MXM], krad, jb, je, ib, ie, mm, nextr, mcnt, i, j, k, cidx,
        cidx1, jj, ii, jq, iq, m, n, ifinct, ier;
/*----------------------------------------------------------------------*/
    *iret = 0;
    *nummin = 0;
    *nummax = 0;

    /*
     * Adjust radius to a sensible value.
     */
    if ( (*ksrad) <= 0 ) {
	krad = 3;
    } else if ( ( (*ksrad) > (*kx) ) ||
	        ( (*ksrad) > (_dgarea.kgxmax - _dgarea.kgxmin) ) ||
		( (*ksrad) > (*ky) ) ||
		( (*ksrad) > (_dgarea.kgymax - _dgarea.kgymin) ) ) {
	krad = G_MIN ( ((*kx)-1)/2,
	       G_MIN ( (_dgarea.kgxmax-_dgarea.kgxmin-1)/2,
	       G_MIN ( ((*ky)-1)/2, (_dgarea.kgymax-_dgarea.kgymin-1)/2 ) ) );
    } else {
	krad = *ksrad;
    }

    /*
     * Check for special wrap-around and pole cases, set calculation area.
     */
    dg_pwts ( kx, ky, &polen, &poles, &wrapa, &wrapc, iret );
    if ( *iret != 0 ) return;

    /*
     * Calculation area will be (ib, jb) to (ie, je).
     */
    if ( poles == G_TRUE && ( _dgarea.kgymin < (1 + krad) ) ) {
	jb = 1;
    } else {
	/*
	 * IF south pole is true, it is not relevant.
	 */
	jb = G_MAX ( (1 + krad), _dgarea.kgymin );
    }

    if ( polen == G_TRUE && ( _dgarea.kgymax > ((*ky) - krad) ) ) {
	je = *ky;
    } else {
	/*
	 * If north pole is true, it is not relevant.
	 */
	je = G_MIN ( ((*ky) - krad), _dgarea.kgymax );
    }

    if ( ( wrapa == G_TRUE || wrapc == G_TRUE ) &&
	 ( ( _dgarea.kgxmin < (1 + krad) ) ||
	   ( _dgarea.kgxmax > ((*kx) - krad) ) ) ) {
	ib = 1;
	if ( wrapa == G_TRUE ) ie = *kx;
	if ( wrapc == G_TRUE ) ie = (*kx) - 1;
    } else {
	ib = G_MAX ( (1 + krad), _dgarea.kgxmin );
	ie = G_MIN ( ((*kx) - krad), _dgarea.kgxmax );
    }

    /*
     * Find the extrema.
     */
    for ( mm = 1; mm <= 2; mm++ ) {
	if ( mm == 1 ) {
	    /*
	     * Finding minima.
	     */
	    rmin = *rlomin;
 	    rmax = *rhimin;
	    nextr = *nmin;
	} else {
	    /*
	     * Finding maxima.
	     */
	    rmin = *rlomax;
	    rmax = *rhimax;
	    nextr = *nmax;
	}

	mcnt = 0;
	for ( j = jb; j <= je; j++ ) {
	    for ( i = ib; i <= ie; i++ ) {
	        cidx = ( j - 1 ) * (*kx) + i - 1;

		/*
		 * Check that reference point is present and visible. 
		 */
		if ( ! ERMISS ( grid[cidx] ) ) {
		    ok = G_TRUE;
		    alleq = G_TRUE;
		    qxm1 = RMISSD;
		    qxp1 = RMISSD;
		    qym1 = RMISSD;
		    qyp1 = RMISSD;

		    jj = j - krad;
		    while ( jj <= (j + krad) && ok == G_TRUE ) {
			jq = jj;

			/*
			 * Accomodate pole.
			 */
			if ( jj > (*ky) ) {
			    jq = 2 * (*ky) - jj;
			    dopole = G_TRUE;
			} else if ( jj < 1 ) {
			    jq = 2 - jj;
			    dopole = G_TRUE;
			} else {
			    dopole = G_FALSE;
			}

			ii = i - krad;
			while ( ii <= (i + krad) && ok == G_TRUE ) {
			    iq = ii;

			    /*
			     * Accomodate pole.
			     */
			    if ( dopole == G_TRUE ) {
				if ( ii > ((*kx)/2) ) {
				    iq = ii - ((*kx)/2);
				} else {
				    iq = ii + ((*kx)/2);
				}
			    }

			    /*
			     * If wrap is true, re-adjust horizontal location.
			     */
			    if ( ii < 1 ) iq = ii + (*kx);
			    if ( ii > (*kx) ) iq = ii - (*kx);

			    /*
			     * Test point (iq, jq) must not equal reference 
			     * point (i,j) or be missing.
			     */
			    cidx1 = ( jq - 1 ) * (*kx) + iq - 1;
			    if ( ( iq != i || jq != j ) &&
				 ( ! ERMISS ( grid[cidx1] ) ) ) {
				/*
				 * Grid points must satisfy extremum condition.
				 */
				if ( mm == 1 ) {
				    if ( grid[cidx] > grid[cidx1] )
					ok = G_FALSE;
				    if ( !G_DIFF(grid[cidx], grid[cidx1]) )
					alleq = G_FALSE;
				} else {
				    if ( grid[cidx] < grid[cidx1] )
					ok = G_FALSE;
				    if ( !G_DIFF(grid[cidx], grid[cidx1]) )
					alleq = G_FALSE;
				}

				/*
				 * Set up points needed for interpolation.
				 */
				if ( ok == G_TRUE && (*intflg) == G_TRUE ) {
				    if ( ii == i-1 && jj == j )
					qxm1 = grid[cidx1];
				    if ( ii == i+1 && jj == j )
					qxp1 = grid[cidx1];
				    if ( jj == j-1 && ii == i )
					qym1 = grid[cidx1];
				    if ( jj == j+1 && ii == i )
					qyp1 = grid[cidx1];
				}
			    }
			    ii++;
			}
			jj++;
		    }

		    if ( ok == G_TRUE && alleq == G_FALSE &&
			 ( ( G_DIFF(rmin, rmax) ) ||
			   ( ( grid[cidx] >= rmin ) &&
			     ( grid[cidx] <= rmax ) ) ) ) {
			xq = i;
			yq = j;
			vq = grid[cidx];

			if ( ( (*intflg) == G_TRUE ) &&
			     ( ( ! ERMISS ( qxm1 ) ) &&
		  	       ( ! ERMISS ( qxp1 ) ) &&
			       ( ! ERMISS ( qym1 ) ) &&
			       ( ! ERMISS ( qyp1 ) ) ) ) {
			    /*
			     * Use linear terms of 2-D Taylor series.
			     */
			    dqdx = .5 * ( qxp1 - qxm1 );
			    dqdy = .5 * ( qyp1 - qym1 );
			    dqdx2 = qxp1 - 2. * vq + qxm1;
			    dqdy2 = qyp1 - 2. * vq + qym1;
			    if ( !G_DIFFT(dqdx2, 0.0F, GDIFFD) &&
				 !G_DIFFT(dqdy2, 0.0F, GDIFFD) ) {
				dx = - dqdx / dqdx2;
				dy = - dqdy / dqdy2 ;
				xq += dx;
				yq += dy;
				vq += dx * dqdx + dy * dqdy;
			    }
			}

			/*
			 * Load each point into array for sorting.
			 */
			if ( mcnt < MXM ) {
			    xextr[mcnt] = xq;
			    yextr[mcnt] = yq;
			    vextr[mcnt] = vq;
			    mcnt++;
			} else {
			    if ( mm == 1 && (*nmin) != 0 ) {
				*iret = 1 ;
			    } else if ( mm == 2 && (*nmax) != 0 &&
				        (*iret) != 1 && (*iret) != 3 ) {
				*iret = 2;
			    } else if ( (*nmin) != 0 && (*nmax) != 0 ) {
				*iret = 3;
			    }
			}
		    }
		}
	    }
	}

	/*
	 * Eliminate points outside visible region.
	 */
	if ( mcnt > 0 ) {
	    gptvis ( sys_G, &mcnt, xextr, yextr, keep, &ier );
	}

	dg_cola ( &mcnt, xextr, yextr, vextr, keep, &ier );

	/*
	 * Bubble sort the extrema.
	 */
	m = mcnt;
	sorted = G_FALSE;
	while ( ( sorted == G_FALSE ) && ( m >= 2 ) ) {
	    sorted = G_TRUE;
	    for ( n = 2; n <= m; n++ ) {
		if ( ( ( mm == 1 ) &&
		       ( vextr[n-2] > vextr[n-1] ) ) ||
		     ( ( mm == 2 ) &&
		       ( vextr[n-2] < vextr[n-1] ) ) ) {
		    temp1 = vextr[n-2];
		    temp2 = xextr[n-2];
		    temp3 = yextr[n-2];
		    vextr[n-2] = vextr[n-1];
		    xextr[n-2] = xextr[n-1];
		    yextr[n-2] = yextr[n-1];
		    vextr[n-1] = temp1;
		    xextr[n-1] = temp2;
		    yextr[n-1] = temp3;
		    sorted = G_FALSE;
		}
	    }
	    m--;
	}

	/*
	 * Check for clusters, collapse clusters to one item. 
	 */
	dg_clmp ( &krad, &mcnt, xextr, yextr, vextr, idofcl, keep, &ier );

	/*
	 * Load the output arrays.
	 */
	ifinct = G_MIN ( mcnt, nextr );
	for ( k = 0; k < ifinct; k++ ) {
	    if ( mm == 1 ) {
		*nummin = ifinct;
		xmin[k] = xextr[k];
		ymin[k] = yextr[k];
		vmin[k] = vextr[k];
	    } else {
		*nummax = ifinct;
		xmax[k] = xextr[k];
		ymax[k] = yextr[k];
		vmax[k] = vextr[k];
	    }
	}
    }

    return;
}
