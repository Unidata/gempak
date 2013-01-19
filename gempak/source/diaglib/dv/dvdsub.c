#include "dv.h"

void dv_dsub  ( int *iret )
/************************************************************************
 * dv_dsub								*
 *									*
 * This subroutine computes the difference in north-relative direction	*
 * between two vectors.							*
 *									*
 *     DSUB ( V1, V2 ) = [ DIRN (V1) - DIRN (V2) ]			*
 *									*
 * DSUB generates a scalar grid.					*
 *									*
 * dv_dsub  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * K. Brill/HPC		 2/03						*
 * S. Gilbert/NCEP	11/05		Translation from Fortran        *
 ************************************************************************/
{
	int		i, ier, kxd, kyd, ksub1, ksub2, zero=0;
	int		num, numu1, numu2, numv1, numv2, ndr1, ndr2;
        float           *grnum, *grndr1, *grndr2, dir1, dir2, ddr;

/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Compute the north relative direction of the first vector.
         */
	dg_getv ( &numu1, &numv1, iret );
	if  ( *iret != 0 )  return;
	dg_putv ( &numu1, &numv1, iret );
	if  ( *iret != 0 )  return;
	dv_dirn (iret);
	if  ( *iret != 0 )  return;

        /*
         *	Get the direction of the first vector.
         */
	dg_gets ( &ndr1, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Compute the north relative direction of the second vector.
         */
	dg_getv ( &numu2, &numv2, iret );
	if  ( *iret != 0 )  return;
	dg_putv ( &numu2, &numv2, iret );
	if  ( *iret != 0 )  return;
	dv_dirn (iret);
	if  ( *iret != 0 )  return;

        /*
         *	Get the direction of the second vector.
         */
	dg_gets ( &ndr2, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Create the next scalar grid.
         */
	dg_nxts ( &num, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Compute the difference.
         */
        dg_getg( &ndr1, &grndr1, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg( &ndr2, &grndr2, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg( &num, &grnum, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    dir1 = grndr1[i];
	    dir2 = grndr2[i];
	    if ( ERMISS (dir1) || ERMISS (dir2) )
		grnum[i] = RMISSD;
	    else {
		ddr = dir1 - dir2;
		if ( ddr <= -180. )
		    ddr = ddr + 360.;
		else if ( ddr > 180. )
		    ddr = ddr - 360.;
		
		grnum[i] = ddr;
	    }
	}

        /*
         *	Make a name of the form 'DSUB'//u1//u2 update both grid
         *	headers; update the stack.
         */
	dg_updh  ( "DSUB", &num, &numu1, &numu2, iret );
	dg_puts  ( &num, iret );
	dg_esub  ( &num, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
