#include "dv.h"

void dv_wnan  ( int *iret )
/************************************************************************
 * dv_wnan								*
 *									*
 * This subroutine computes a hyperbolic wind field whose axis of	*
 * dilatation is oriented S degrees from the grid X axis:		*
 *									*
 *     WNAN ( S ) = [ SP * ( 2 * y * cos (S) * sin (S)			*
 *                           - x * ( sin (S) ** 2 - cos (S) ** 2 ),	*
 *                    SP * ( 2 * x * cos (S) * sin (S)			*
 *                           + y * ( sin (S) ** 2 - cos (S) ** 2 ) ]	*
 *									*
 *                  where:  SP = .2 (20 m/s per 100 km)			*
 *                          x, y are zero at the grid center		*
 *									*
 * The equation is derived by rotating the streamfunction		*
 *									*
 *     z = -C * x' * y'							*
 *									*
 * (C a constant, x' parallel to the axis of dilatation) through the	*
 * angle S to the grid orientation and computing velocity.		*
 *									*
 * A CED projection is implicitly assumed.  WNAN generates a vector	*
 * field.								*
 *									*
 * dv_wnan  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * G. Huffman/GSC	 4/89						*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * T. Lee/GSC		 5/96   Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		 5/02	Eliminate LLMXGD declarations in DGCMN	*
 *				by using internal grids for lat/lon	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
	const int	zero = 0;
	int		i, j, k, ier, nval, kxd, kyd, ksub1, ksub2;
	int		num, numu, numv, idglat, idglon;
	float		*grnum, *gru, *grv, *grlat, *grlon;
        int             icntr;
        float           delx, dely, sp, s, x, y, cs2, s2c2, xd0, yd0;

/*----------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the scalar grid into grid table.
         */
	dg_gets  ( &num, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Find the grid center (ICNTR is approx.) and grid spacing in km.
         */
	dg_ltln  ( iret );
	if  ( *iret != 0 ) return;

        nval = 1;
        dg_iget ( "IDGLAT", &nval, &idglat, iret );
        dg_iget ( "IDGLON", &nval, &idglon, iret );
        dg_getg( &idglat, &grlat, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg( &idglon, &grlon, &kxd, &kyd, &ksub1, &ksub2, iret );

	xd0   = ( kxd + 1 ) / 2.;
	yd0   = ( kyd + 1 ) / 2.;
	icntr = ceil ( xd0 ) + ceil ( yd0 - 1. ) * kxd;
	delx  = ( grlon [ icntr ]  - grlon [ icntr-1 ] ) *
     		RTD * 111. * cos ( grlat[icntr-1] );
	dely  = ( grlat [ icntr + kxd -1] - grlat [ icntr-1 ] ) *
     		RTD * 111. ;

        /*
         *	Get the next two grid numbers for the output.
         *	Set the speed coefficient (20 m/s per 100 km).
         *	Set the 1-D index; the DO's are 2-D to ease programming.  
         *	Do the velocity calculation.
         */
	dg_nxtv  ( &numu, &numv, iret );
	if  ( *iret != 0 ) return;
	sp = .2;

        dg_getg( &num, &grnum, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg( &numu, &gru, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg( &numv, &grv, &kxd, &kyd, &ksub1, &ksub2, iret );

        i = 0;
	for ( k = 1; k <= kyd; k++ ) {
	for ( j = 1; j <= kxd; j++ ) {
	    s = grnum[i];
	    if  ( ERMISS ( s ) )  {
		gru[i] = RMISSD;
		grv[i] = RMISSD;
            }
	    else {

                /*
                 *	Convert S to radians.
                 */
		s    = s * DTR;
		x    = ( j - xd0 ) * delx;
		y    = ( k - yd0 ) * dely;
		cs2  = cos ( s ) * sin ( s ) * 2.;
		s2c2 = 1. - 2. * cos ( s ) * cos ( s );
		gru[i] = sp * ( y * cs2 - x * s2c2 );
		grv[i] = sp * ( x * cs2 + y * s2c2 );
            }
	    
	    i++;
	}
	}

        /*
         *	Make a name of the form 'WA'//S and update the header;
         *	update the stack.
         */
	dg_updv  ( "WA", &numu, &numv, &num, &zero, iret );
	dg_putv  ( &numu, &numv, iret );
	dg_esub  ( &numu, &numv, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
