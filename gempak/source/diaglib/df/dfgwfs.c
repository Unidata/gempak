#include "df.h"

static const int MAXWTS = 100;

void df_gwfs ( int *iret )
/************************************************************************
 * DF_GWFS								*
 *									*
 * This subroutine smoothes a scalar grid using a moving average	*
 * low-pass filter whose weights are determined by the normal 		*
 * (Gaussian) probability distribution function for two dimensions.	*
 * The weight given to any grid point within the area covered by the	*
 * moving average for a	target grid point is proportional to		*
 *									*
 *		    EXP [ -( D ** 2 ) ],				*
 *									*
 * where D is the distance from	that point to the target point divided	*
 * by the standard deviation of the normal distribution.  The value of	*
 * the standard deviation is determined by the degree of filtering	*
 * requested.  The degree of filtering is specified by an integer.	*
 * This integer is the number of grid increments from crest to crest	*
 * of the wave for which the theoretical response is 1/e = .3679.  If 	*
 * the grid increment is called delta_x, and the value of this integer	*
 * is represented by N, then the theoretical filter response function 	*
 * value for the N * delta_x wave will be 1/e.  The actual response 	*  
 * function will be greater than the theoretical value.			*
 *									*
 * The larger N is, the more severe the filtering will be, because the	*
 * response function for all wavelengths shorter than N * delta_x	*
 * will be less than 1/e.  Furthermore, as N is increased, the slope	*
 * of the filter response function becomes more shallow; so, the	*
 * response at all wavelengths decreases, but the amount of decrease	*
 * lessens with increasing wavelength.  (The theoretical response	*
 * function can be obtained easily--it is the Fourier transform of the	*
 * weight function described above.)					*
 *									*
 * The area of the patch covered by the moving average varies with N.	*
 * As N gets bigger, the smoothing gets stronger, and weight values	*
 * farther from the target grid point are larger because the standard	*
 * deviation of the normal distribution is bigger.  Thus, increasing	*
 * N has the effect of expanding the moving average window as well as	*
 * changing the values of weights.  The patch is a square covering all	*
 * points whose weight values are within two standard deviations of the	*
 * mean of the two dimensional normal distribution.  The leftover	*
 * weight values representing the fringe of the distribution are	*
 * applied to the target grid point.  This has the effect of increasing	*
 * the response function over the theoretical value.			*
 *									*
 * When this function is invoked, the first argument is the grid to be	*
 * smoothed, the second is the value of N as described above:		*
 *									*
 *			GWFS ( S, N )					* 
 *									*
 * where N > 1.  If N <= 1, N = 2 is assumed.  For example, if N = 4,	*
 * then the 4 delta x wave length is passed with approximate response	*
 * 1/e.									*
 *									*
 * DF_GWFS  ( IRET )							*
 *									*
 * Output parameters:							*
 *	IRET		INTEGER		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * K. Brill/NMC		 2/95						*
 * K. Brill/NMC		 4/95	Added SUBA and documentation		*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 * K. Brill/HPC         01/06   Increase footprint; assign RMISSD	*
 ************************************************************************/
{
    int ni, no, nnw,  kxd, kyd, ksub1, ksub2, zero, ier;
    int nwl, nr, jw, iw, jj, ii, is, ie, js, je, j, i, indx;
    float *gnnw, *gnist, *gnost;
    float sgma, sumw, sumf, sig2, aa, x, y;
    float w[MAXWTS][MAXWTS];
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get the input grid number.
     */
    dg_gets ( &ni, iret );
    if ( *iret != 0 ) return;

    /*
     * Get multiple of delta x of wavelength having 1/e response.
     */
    dg_gets ( &nnw, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &nnw, &gnnw, &kxd, &kyd, &ksub1, &ksub2, iret );
    nwl = G_NINT ( gnnw[0] );
    if ( nwl <= 1 ) nwl = 2;

    /*
     * Get a new grid number for the output.
     */
    dg_nxts ( &no, iret );
    if ( *iret != 0 ) return;

    /*
     * Compute the array of weights.
     *
     * The range of the filter is twice the standard deviation of the
     * required Gaussian distribution.
     */
    sgma = (float)nwl / ( PI * sqrt ( 2.0 ) );
    nr = G_NINT ( 2. * sgma );
    if ( nr < 1 ) nr = 1;
    if ( nr >= MAXWTS ) nr = MAXWTS - 1;

    /*
     * Compute the matrix of weights for one quadrant using symmetry
     * of two dimensional Gaussian surface.
     */
    sumw = 0.0;
    sig2 = sgma * sgma;
    aa = 1. / ( sig2 * PI );
    for ( jw = 1; jw <= nr + 1; jw++ ) {
        if ( jw == 1 ) {
	    is = 2;
        } else {
	    is = jw;
        }
        for ( iw = is; iw <= nr + 1; iw++ ) {
	    x = iw - 1;
	    y = jw - 1;
	    w[iw-1][jw-1] = aa * exp ( - ( x*x + y*y ) / sig2 );
	    w[jw-1][iw-1] = w[iw-1][jw-1];
	    if ( jw == 1 || jw == iw ) {
	        sumw += w[iw-1][jw-1];
	    } else {
	        sumw += 2. * w[iw-1][jw-1];
  	    }
        }
    }
    sumw *= 4.;
    w[0][0] = 1. - sumw;

    /*
     * Apply weights over non-missing values.
     */
    dg_getg ( &ni, &gnist, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &no, &gnost, &kxd, &kyd, &ksub1, &ksub2, iret );

    for ( jj = 1; jj <= kyd; jj++ ) {
        for ( ii = 1; ii <= kxd; ii++ ) {
	    is = ii - nr;
	    ie = ii + nr;
	    js = jj - nr;
	    je = jj + nr;
	    sumw = 0.0;
	    sumf = 0.0;
	    for ( j = js; j <= je; j++ ) {
	        if ( j >= 1 && j <= kyd ) {
		    for ( i = is; i <= ie; i++ ) {
		        if ( i >= 1 && i <= kxd ) {
			    iw = abs (i-ii) + 1;
			    jw = abs (j-jj) + 1;
			    indx = (j-1)*kxd + i;
			    if ( ! ERMISS ( gnist[indx-1] ) ) {
			        sumw += w[iw-1][jw-1];
			        sumf += gnist[indx-1] * w[iw-1][jw-1];
			    }
		        }
		    }
	        }
	    }
	    indx = ( jj - 1 ) * kxd + ii;
	    if ( !G_DIFFT(sumw, 0.0F, GDIFFD) && ! ERMISS ( gnist[indx-1] ) ) {
	        gnost[indx-1] = sumf / sumw;
	    } else {
	        gnost[indx-1] = RMISSD;
	    }
        }
    }

    /*
     * Make a name of the form 'GWF'//S and update header;
     * update stack.
     */
    dg_updh ( "GWF", &no, &ni, &zero, iret );
    dg_puts ( &no, iret );
    dg_esub ( &no, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
