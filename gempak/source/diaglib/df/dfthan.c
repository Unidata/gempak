#include "df.h"

void df_than ( int *iret )
/************************************************************************
 * df_than								*
 *									*
 * This subroutine computes a THTA field whose isentropes are rotated	*
 * S degrees from the grid X axis:					*
 *									*
 *     THAN ( S ) = TH0 + TH * ( x * sin (S) - y * cos (S) )		*
 *									*
 *                  where:  TH0 = 270 K					*
 *                          TH  = .1 (10 K per 100 km)			*
 *                          x, y are zero at the grid center		*
 *									*
 * The equation is derived by rotating the function			*
 *									*
 *     THTA = TH0 - TH * y'							*
 *									*
 * (i.e., warm to the "south") through the angle S.			*
 *									*
 * A CED projection is implicitly assumed.				*
 *									*
 * df_than ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * G. Huffman/GSC	 4/89						*
 * T. Lee/GSC		 4/96	Single dimension for dgg		*
 * K. Tyle/GSC           5/96   Moved IGDPT outside do-loop             *
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		 5/02	Eliminate LLMXGD declarations in DGCMN	*
 *				by using internal grids for lat/lon	*
 * R. Tian/SAIC		11/05	Recoded from Fortran			*
 ************************************************************************/
{
    int num, numt, nval, idglat, idglon, kxd, kyd, ksub1, ksub2, icntr,
        i, j, k, ier, zero;
    float *gnum, *gnumt, *glat, *glon, xd0, yd0, delx, dely, th0, th,
        s, x, y;
/*----------------------------------------------------------------------*/
    *iret = 0;
    zero = 0;

    dg_ssub ( iret );

    /*
     * Get one grid from the stack.
     */
    dg_gets ( &num, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &num, &gnum, &kxd, &kyd, &ksub1, &ksub2, iret );

    /*
     * Find the grid center (ICNTR is approx.) and grid spacing in km.
     */
    dg_ltln ( iret );
    if ( *iret != 0 ) return;

    nval = 1;
    dg_iget ( "IDGLAT", &nval, &idglat, iret );
    dg_iget ( "IDGLON", &nval, &idglon, iret );
    dg_getg ( &idglat, &glat, &kxd, &kyd, &ksub1, &ksub2, iret );
    dg_getg ( &idglon, &glon, &kxd, &kyd, &ksub1, &ksub2, iret );

    xd0 = ( kxd + 1 ) / 2.;
    yd0 = ( kyd + 1 ) / 2.;
    icntr = G_NINT ( xd0 ) + G_NINT ( yd0 - 1. ) * kxd;
    delx = ( glon[icntr] - glon[icntr-1] ) * RTD * 111. * 
           cos ( glat[icntr-1] );
    dely = ( glat[icntr+kxd-1] - glat[icntr-1] ) * RTD * 111.;

    /*
     * Get the next grid number for the output.
     * Set the coefficients (270 K and 20 K per 100 km).
     * Set the 1-D index; the DO's are 2-D to ease programming.  
     * Do the THTA calculation.
     */
    dg_nxts ( &numt, iret );
    if ( *iret != 0 ) return;
    dg_getg ( &numt, &gnumt, &kxd, &kyd, &ksub1, &ksub2, iret );

    th0 = 270.;
    th  = .1;
    i = 0;
    for ( k = 1; k <=  kyd; k++ ) {
	for ( j = 1; j <= kxd; j++ ) {
	    s = gnum[i];
	    if ( ERMISS ( s ) ) {
		gnumt[i] = RMISSD;
	    } else {
		/*
		 * Convert S to radians.
		 */
		s *=  DTR;
		x = ( j - xd0 ) * delx;
		y = ( k - yd0 ) * dely;
		gnumt[i] = th0 + th * ( x * sin ( s ) - y * cos ( s ) );
	    }
	    i++;
	}
    }

    /*
     * Get a name of the form 'TA'//S and update header;
     * update stack.
     */
    dg_updh ( "TA", &numt, &num, &zero, iret );
    dg_puts ( &numt, iret );
    dg_esub ( &numt, &zero, &zero, &zero, &ier );
    if ( ier != 0 ) *iret = ier;

    return;
}
