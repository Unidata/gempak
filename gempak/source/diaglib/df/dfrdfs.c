#include "df.h"

void df_rdfs ( int *iret )
/************************************************************************
 * DF_RDFS (Resolution Dependent Filter for Scalar)			*
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
 * requested.  The degree of filtering is specified by giving an 	*
 * effective resolution in km for the output grid.  From this value,	*
 * an integer required as the input for the GWFS function is computed.  *
 *									*
 * See the documentation for the GWFS function for more details.	*
 *									*
 * When this function is invoked, the first argument is the grid to be	*
 * smoothed, the second is the effective resolution as described above:	*
 *									*
 *			RDFS ( S, dx )					* 
 *									*
 * where dx > 0.  If the value of dx is less than the grid spacing	*
 * on the internal grid, no filtering is done.				*
 *									*
 * DF_RDFS  ( IRET )							*
 *									*
 * Output parameters:							*
 *	IRET		INTEGER		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * K. Brill/HPC         05/12   Developed from DF_GWFS			*
 ************************************************************************/
{
    int nnw, kxd, kyd, ksub1, ksub2, zero, ier;
    int jj, ii, indx;
    int ixm, iym, ni, no;
    float *gnnw, *gnost;
    float gdx, gdy, dsg, eres, swl;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Compute map scale factors and grid increments.
     */
    dg_mscl ( iret );
    if ( *iret != 0 ) return;

    /*
     * Get the grid spacing values:
     */
    dg_qmsl ( &ixm, &iym, &gdx, &gdy, iret );
    if ( *iret != 0 ) return;
    if ( gdx > gdy ) {
	dsg = gdx;
    } else {
	dsg = gdy;
    }
    dsg = dsg / 1000.0;

    /*printf ("  dsg = %f\n", dsg ); */

    /*
     * Get the input grid number.
     */
    dg_gets ( &ni, iret );
    if ( *iret != 0 ) return;

    /*
     * Get the user specified effective resolution (km).
     */
    dg_gets ( &nnw, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &nnw, &gnnw, &kxd, &kyd, &ksub1, &ksub2, iret );
    eres = gnnw[0];

    if ( eres < dsg ) {
	/*printf ( " No smoothing\n" );*/
        /*
	 *  Do nothing -- return original grid without smoothing.
	 */
        /*
         * Make a name of the form 'RDF'//S and update header;
         * update stack.
         */
        dg_updh ( "RDF", &ni, &ni, &zero, iret );
        dg_puts ( &ni, iret );
        dg_esub ( &ni, &zero, &zero, &zero, &ier );
        if ( ier != 0 ) *iret = ier;
        return;
    } else {
	/*
	 * Call the GWFS program to smooth the grid. The smoother
	   footprint is chosen so as so suppress the 2 delta X
	   wave on the coarse grid to 1/e of the original amplitude.
	 */
	swl = (float)G_NINT ( ( eres / dsg ) * 2.0 );
	/*printf (" Smooth with footprint = %f\n", swl);*/
        /*
         * Get a new grid number for the output.
         */
        dg_nxts ( &no, iret );
        if ( *iret != 0 ) return;
        dg_getg ( &no, &gnost, &kxd, &kyd, &ksub1, &ksub2, iret );
        for ( jj = 1; jj <= kyd; jj++ ) {
            for ( ii = 1; ii <= kxd; ii++ ) {
	        indx = ( jj - 1 ) * kxd + ii;
	        gnost[indx-1] = swl;
	    }
	}
	/*
	 * Put two grids on the stack for the Gaussing weighted filter.
	 */
	dg_puts ( &no, iret );
        if ( *iret != 0 ) return;
	dg_puts ( &ni, iret );
        if ( *iret != 0 ) return;
	df_gwfs ( iret );
    }
    return;
}
