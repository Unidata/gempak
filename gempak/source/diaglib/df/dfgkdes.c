#include "df.h"

void df_gkdes ( int *iret )
/************************************************************************
 * DF_GKDES								*
 *									*
 * This subroutine smoothes a scalar grid using a moving average	*
 * low-pass filter whose weights are determined by the normal 		*
 * (Gaussian) probability distribution function for two dimensions.	*
 *									*
 * A description of the 2-D Gaussian Kernel Density Estimation (GKDE)   *
 * can be found by referring to the GWFS subroutine.  This particular   *
 * function is different in two key ways, though.  First, the logic     * 
 * used here is more efficient since the partial weights are calcuated  *
 * in isolation for the window constructed around each grid point.      *
 * This permits skipping of zero values and missing data points to speed*
 * up the process of smoothing the scalar field. Second, the number of  *
 * SIGMA (i.e. standard deviations) used to construct the window is not *
 * fixed.								*
 *                                                                      *
 * When this function is invoked, the first argument is the scalar      *
 * field (S) to be smoothed, the second is SIGMA (standard deviation;   *
 * in grid points ), and the third is the factor of smoothing           *
 *(i.e. number of SIGMA):                                               *
 *			GKDES ( S, SIGMA, FACTOR )		        * 
 *									*
 * where SIGMA >= 1 and FACTOR can be fractions of SIGMAs.              *
 * If SIGMA < 1, SIGMA = 1 is assumed.  If FACTOR = 0, no smoothing     *
 * is performed.                                                        *
 *									*
 * DF_GKDES  ( IRET )							*
 *									*
 * Output parameters:							*
 *	IRET		INTEGER		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * C. Melick & P. Marsh/SPC   09/16					*
 ************************************************************************/
{
    int ni, no, nsig, nfac, kxd, kyd, ksub1, ksub2, zero, ier;
    int ixmscl, iymscl, nxx, nyy, nw;
    int ng, ngn, nx, ny, jj, ii, is, ie, js, je, j, i, indx;
    float *gsig, *gfac, *gnist, *gnost, *partweight;
    float sig, sig2, gddx, gddy, ng2, factor, dist2, amp;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get the input grid number.
     */
    dg_gets ( &ni, iret );
    if ( *iret != 0 ) return;

    dg_getg ( &ni, &gnist, &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Get sigma (in grid points).
     */
    dg_gets ( &nsig, iret );
    if ( *iret != 0 ) return;

    dg_getg ( &nsig, &gsig, &kxd, &kyd, &ksub1, &ksub2, iret );
    sig = gsig[0];

    /*
     * Get smoothing factor (# of sigma).
     */
    dg_gets ( &nfac, iret );
    if ( *iret != 0 ) return;

    dg_getg ( &nfac, &gfac, &kxd, &kyd, &ksub1, &ksub2, iret );
    factor =  gfac[0];

    if ( sig < 1 ) sig = 1;  /* Sigma = 1 grid point */

    if ( factor < 0 ) factor = 0;  /*  No smoothing is performed */ 

    /*
     * Get a new grid number for the output.
     */
    dg_nxts ( &no, iret );
    if ( *iret != 0 ) return;

    dg_getg ( &no, &gnost, &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Initialize output grid.
     */

    for ( indx = 0; indx < kxd*kyd; indx++ ) {
        if ( ERMISS ( gnist[indx] ) ) {
              gnost[indx] = RMISSD;
        } else {
              gnost[indx] = 0;
        }
    }



     /*          printf ("\ngddx = %f\n",gddx);  
                printf ("\nsigma =  %f\n",sig);
                printf ("\nfactor = %f\n",factor); */
    if ( factor == 0 ) {
      for ( indx = 0; indx < kxd*kyd; indx++ ) {
        if ( ! ERMISS ( gnist[indx] ) ) {
          gnost[indx] = gnist[indx];
        }
      }
    } else {
      sig2 = sig*sig;
      ng = G_NINT (factor * sig);

      ng2 = (float)(ng * ng);
      ngn = -1 * ng;
      nx = 2*ng+1;
      ny = 2*ng+1;

    /*
     * Allocate space and initialize array for weights.
     */

      G_MALLOC (partweight, float, nx*ny+1, "GKDES: weights");

      for ( nw = 0; nw < nx*ny; nw++ ) {
            partweight[nw] = 0.;
      }
                  
    /*
     * Compute the array of weights.
     */

      nw=-1;
      for (nyy = ngn; nyy < ng+1; nyy++) {
        for (nxx = ngn; nxx < ng+1; nxx++) {
            nw = nw+1;
            dist2 = (float)(nxx*nxx) + (float)(nyy*nyy);
            if ( dist2 <= ng2 ) {
                partweight[nw] = exp(-0.5*dist2/sig2);
      /*         printf ("\npartweight at index: %d is %f\n",nw,partweight[nw]); */
            }
        }
      }

    /*
     * Apply weights over non-missing values.
     */

      for ( j = 1; j <= kyd; j++ ) {
         for ( i = 1; i <= kxd; i++ ) {
            indx = (j-1)*kxd + i;
            if ( ! ERMISS ( gnist[indx-1] ) && gnist[indx-1] > 0 ) {
                amp = gnist[indx-1] / (2*PI*sig2);
                is=i-ng;
                ie=i+ng;
                js=j-ng;
                je=j+ng;
                nw = -1;
	        for ( jj = js; jj <= je; jj++ ) {
	           if ( jj >= 1 && jj <= kyd ) {
		     for ( ii = is; ii <= ie; ii++ ) {
		        if ( ii >= 1 && ii <= kxd ) {
                           nw += 1;
                           indx = (jj-1)*kxd + ii;
                           if ( ! ERMISS ( gnist[indx-1] ) ) {
                              gnost[indx-1] = gnost[indx-1] + amp*partweight[nw];
                           }
                        }
                     }
                   }
                }
            }
         }
      }

    /*
     * Free memory space for weights.
     */
      
      G_FREE(partweight,float);
    }


    /*
     * Make a name of the form 'GKDES'//S and update header;
     * update stack.
     */
    dg_updh ( "GKDES", &no, &ni, &zero, iret );
    dg_puts ( &no, iret );
    dg_esub ( &no, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
