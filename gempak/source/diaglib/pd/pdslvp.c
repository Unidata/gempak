#include "pd.h"

static const int maxknt = 512;

void pd_slvp ( const float *frc, const float *ges, const int *itypbc,
               const float *rmx, const float *rmy, const int *kxx,
	       const int *kyy, const float * hddx, const float *hddy,
	       float *aa, float *bb, float *fld, int *iret ) 
/************************************************************************
 * pd_slvp                                                              *
 *                                                                      *
 * This subroutine solves a Poisson equation by successive over-        *
 * relaxation.  The boundary conditions are assumed to be set in the    *
 * GES grid.  If ITYPBC is zero, Dirichlet boundary conditions apply    *
 * and the solution is done over all interior grid points.  If ITYPBC   *
 * is one, Dirichlet boundary conditions apply but the solution is done *
 * over all points interior to the two outer boundary points.  This     *
 * preserves the tangential and normal derivatives set up in the        *
 * GES grid.                                                            *
 *                                                                      *
 * The convergence criterion is that the average magnitude of the       *
 * residual be less than two percent of the average magnitude of the    *
 * forcing function.                                                    *
 *                                                                      *
 * Note:  This calculation is done only on the whole grid--no grid      *
 *        subsetting is allowed.                                        *
 *                                                                      *
 * pd_slvp ( frc, ges, itypbc, rmx, rmy, kx, ky, hddx, hddy, aa, bb,    *
 *           fld, iret )                                                *
 *                                                                      *
 * Input parameters:                                                    *
 *      *frc		const float	Forcing function                *
 *      *ges		const float	Guess field                     *
 *      *itypbc         const int	Type of boundary conditions     *
 *                                        0 = Dirichlet                 *
 *                                        1 = Alternate Dirichlet       *
 *      *rmx		const float	Map scale factors along x       *
 *      *rmy		const float	Map scale factors along y       *
 *      *kx             const int	Number of grid points in x      *
 *      *ky             const int	Number of grid points in y      *
 *      *hddx           const float	Grid spacing in x (m)           *
 *      *hddy           const float	Grid spacing in y (m)           *
 *                                                                      *
 * Required scratch arrays:                                             *
 *      *aa		float		Scratch work array              *
 *      *bb		float		Scratch work array              *
 *                                                                      *
 * Output parameters:                                                   *
 *      *fld		float		Solution field                  *
 *      *iret           int		Return code                     *
 *                                        0 = normal return             *
 *                                       -1 = convergence failed        *
 **                                                                     *
 * Log:                                                                 *
 * K. Brill/NMC         12/92                                           *
 * K. Brill/NMC         01/93   Use immediate update technique          *
 * S. Jacobs/EAI         2/93   Eliminated ref to Neumann cond          *
 * T. Lee/GSC           12/98   Fixed FORMAT statement, X -> 1X         *
 * J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *
 *                              DATA statement                          *
 * K. Brill/HPC          5/02   Use input scratch arrays not local ones *
 * R. Tian/SAIC		 9/05	Translated from FORTRAN			*
 ************************************************************************/
{
    float gddx, gddy, alpha, sum, avgfrc, dx2, ymsdx, xmsdx, rmscl, qmscl,
          dy2, xmsdy, ymsdy, dydy, dxdx, ressum, d1, d2, d2x2, d2y2,
	  d1x1, d1y1, rsd, abc, resavg, percnt;
    int kx, ky, kxky, knt, kxm1, ip1, im1,indx, kym1, im, ip, i1, j1, i2,
        j2, kntres, iii, jm1, jp1, ii, jj;
    int done;
/*----------------------------------------------------------------------*/
    *iret = 0;
    gddx = *hddx;
    gddy = *hddy;
/*
 * Set initial over-relaxation coefficient.
 */
    alpha = 1.8F;
/*
 * Load the guess into the solution and compute average forcing.
 */
    kx = *kxx;
    ky = *kyy;
    kxky = kx * ky;
    sum = 0.0F;
    knt = 0;
    avgfrc = 1.0F;
    for ( ii = 0; ii < kxky; ii++ ) {
        fld[ii] = ges[ii];
	if ( ! ERMISS ( frc[ii] ) ) {
	    sum += fabs ( frc[ii] );
	    knt++;
	}
    }
    if ( knt != 0 ) avgfrc = sum / (float) ( knt );
    if ( G_DIFF(avgfrc, 0.0F) ) avgfrc = 1.0F;
/*
 * Evaluate map scale factor derivative expressions which are the
 * coefficients of the first derivative terms.
 *
 * Compute the terms involving derivatives w.r.t. x.
 */
    dx2 = 2.0F * gddx;
    kxm1 = kx - 1;
/*
 * Loop over all grid rows.
 */
    for ( jj = 1; jj <= ky; jj++ ) {
/*	   
 * Loop over interior grid points in row j.
 */
	for ( ii = 2; ii <= kxm1; ii++ ) {
	    ip1 = ii + 1 + (jj - 1) * kx;
	    im1 = ip1 - 2;
	    indx = ip1 - 1;
	    ymsdx = ( rmy[ip1-1] - rmy[im1-1] ) * rmx[indx-1] * 
     	            rmx[indx-1] / ( rmy[indx-1] * dx2 );
	    xmsdx = ( rmx[ip1-1] - rmx[im1-1] ) * rmx[indx-1] / dx2;
	    aa[indx-1] = xmsdx - ymsdx;
        }
/*
 * Compute one-sided difference at the beginning of row j.
 */
        im1 = 1 + (jj - 1) * kx;
	ip1 = im1 + 1;
	rmscl  = 0.5F * ( rmx[im1-1] + rmx[ip1-1] );	
	qmscl  = 0.5F * ( rmy[im1-1] + rmy[ip1-1] );
	ymsdx  = ( rmy[ip1-1] - rmy[im1-1] ) * rmscl * rmscl /
	         ( qmscl * gddx );
	xmsdx  = ( rmx[ip1-1] - rmx[im1-1] ) * rmscl / ( gddx );
	aa[im1-1] = xmsdx - ymsdx;
/*
 * Compute one-sided difference at the end of row j.
 */
        ip1 = jj * kx;
	im1 = ip1 - 1;
        rmscl  = 0.5F * ( rmx[im1-1] + rmx[ip1-1] );
        qmscl  = 0.5F * ( rmy[im1-1] + rmy[ip1-1] );
	ymsdx = ( rmy[ip1-1] - rmy[im1-1] ) * rmscl * rmscl /
	        ( qmscl * gddx );
	xmsdx = ( rmx[ip1-1] - rmx[im1-1] ) * rmscl / ( gddx );
	aa[ip1-1] = xmsdx - ymsdx;
    }
/*
 * Compute the terms involving derivatives w.r.t. y.
 */
    dy2  = 2.0F * gddy;;
    kym1 = ky - 1;
/*
 * Compute derivative on internal rows.
 */
    for ( jj = 2; jj <= kym1; jj++ ) {
	for ( ii = 1; ii <= kx; ii++ ) {
            im = ii + (jj - 2) * kx;
	    ip = im + 2 * kx;
	    indx = im + kx;
	    xmsdy = ( rmx[ip-1] - rmx[im-1] ) * rmy[indx-1] *
                    rmy[indx-1] / ( rmx[indx-1] * dy2 );
	    ymsdy = ( rmy[ip-1] - rmy[im-1] ) * rmy[indx-1] / ( dy2 );
	    bb[indx-1] = ymsdy - xmsdy;
        }
    }
/*
 * Compute one-sided derivatives along bottom row.
 */
    for ( ii = 1; ii <= kx; ii++ ) {
	im = ii;
	ip = ii + kx;
	indx = ii;
	rmscl  = 0.5F * ( rmy[ip-1] + rmy[im-1] );
	qmscl  = 0.5F * ( rmx[ip-1] + rmx[im-1] );
	xmsdy  = ( rmx[ip-1] - rmx[im-1] ) * rmscl * rmscl /
	         ( qmscl * gddy );
	ymsdy  = ( rmy[ip-1] - rmy[im-1] ) * rmscl / ( gddy );
	bb[indx-1] = ymsdy - xmsdy;
/*
 * Compute one-sided derivative along top row.
 */	  
	im = ii + (kym1 - 1) * kx;
        ip = im + kx;
	indx = ip;
	rmscl  = 0.5F * ( rmy[ip-1] + rmy[im-1] );
	qmscl  = 0.5F * ( rmx[ip-1] + rmx[im-1] );
	xmsdy  = ( rmx[ip-1] - rmx[im-1] ) * rmscl * rmscl /
	         ( qmscl * gddy );
	ymsdy  = ( rmy[ip-1] - rmy[im-1] ) * rmscl / ( gddy );
	bb[indx-1] = ymsdy - xmsdy;
    }
/*
 * Set bounding indexes to apply requested boundary conditions.
 */
    if ( (*itypbc) == 0 ) {
/*
 * Dirichlet conditions.
 */
	i1 = 2;
	i2 = kx - 1;
	j1 = 2;
	j2 = ky - 1;
    } else {
/*
 * Alternate Dirichlet conditions.
 */
	i1 = 3;
	i2 = kx - 2;
	j1 = 3;
	j2 = ky - 2;
    }
    knt = 0;
    dydy = gddy * gddy;
    dxdx = gddx * gddx;
    done = G_FALSE;
    while ( ! done ) {
        knt++;
	ressum = 0.0F;
	kntres = 0;
/*
 * Evaluate interior Laplacian of solution field.
 * Calulate the residual at each point.
 */
	for ( jj = j1; jj <= j2; jj++ ) {
	    for ( ii = i1; ii <= i2; ii++ ) {
	        im1 = ( jj - 1 ) * kx + ii - 1;
		ip1 = ( jj - 1 ) * kx + ii + 1;
		iii = ( jj - 1 ) * kx + ii;
		jm1 = ( jj - 2 ) * kx + ii;
		jp1 = jj * kx + ii;
		if ( ! ERMISS ( fld[ip1-1] ) && 
      		     ! ERMISS ( fld[iii-1] ) &&
      		     ! ERMISS ( fld[im1-1] ) && 
      		     ! ERMISS ( fld[jp1-1] ) &&
      		     ! ERMISS ( fld[jm1-1] ) &&
      		     ! ERMISS ( frc[iii-1] ) ) {
		    d1 = rmx[iii-1] * rmx[iii-1] / dxdx;
		    d2 = rmy[iii-1] * rmy[iii-1] / dydy;
		    d2x2 = d1 * ( fld[ip1-1] - 2.0F * fld[iii-1] +
		           fld[im1-1] );
		    d2y2 = d2 * ( fld[jp1-1] - 2.0F * fld[iii-1] +
		           fld[jm1-1] );
		    d1x1 = ( aa[iii-1] / dx2 ) * ( fld[ip1-1] -
		           fld[im1-1] );
		    d1y1 = ( bb[iii-1] / dy2 ) * ( fld[jp1-1] -
		           fld[jm1-1] );
		    rsd  = d2x2 + d2y2 + d1x1 + d1y1 - frc[iii-1];
/*
 * Update immediately.
 */
		    abc = alpha / ( 2.0F * ( d1 + d2 ) );
		    fld[iii-1] += rsd * abc;
		    ressum += fabs ( rsd );
		    kntres++;
		}
	    }
	}
/*
 * Check for convergence.
 */
	if ( kntres == 0 ) kntres = 1;
	resavg = ressum / (float)kntres;
        percnt = 100.0F * resavg / avgfrc;
	if ( percnt >= 2.0F ) {
/*
 * Keep going.
 */
	    alpha = 1.5F + percnt / 100.0F;
	    if ( alpha > 2.0F ) alpha = 1.95F;
        } else {
            done = G_TRUE;
	    printf ( "  Solver did %5d iterations. %% error = %5.2f"
	             ", Average error = %13.5E\n", knt, percnt, resavg );
	}
	if ( knt > maxknt ) {
	    done = G_TRUE;
	    *iret = -1;
	    printf ( "  Solver did NOT converge. %% error = %f6.2\n",
	             percnt );
	}
    }
}
