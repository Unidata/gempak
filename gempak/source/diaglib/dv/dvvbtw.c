#include "dv.h"

void dv_vbtw ( int *iret )
/************************************************************************
 * dv_vbtw								*
 *									*
 * This subroutine finds values of the magnitude of V which are between *
 * S1 and S2.								*
 *									*
 *     VBTW (V, S1, S2) IF S1 < |V| < S2 OR				*
 *		           S2 < |V| < S1 THEN V ELSE RMISSD		*
 *									*
 * dv_vbtw  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETS			*
 **									*
 * Log:									*
 * S. Maxwell/GSC        8/97                                           *
 * S. Maxwell/GSC        8/97     Expanded gt and lt checking; 		*
 *                                corrected documentation		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB, DG_ESUB; CHK iret & RTRN	*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
	const int	zero=0;
	int		i, ier, kxd, kyd, ksub1, ksub2;
        int             numu, numv, num1, num2;
        float           *grnumu, *grnumv, *grnum1, *grnum2;
        int             nu, nv, nmag;
        float           *gru, *grv, *grmag;

/*----------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the vector and the scalar.
         */
	dg_getv ( &numu, &numv, iret );
	if ( *iret != 0 ) return;
	dg_gets ( &num1, iret );
	if ( *iret != 0 ) return;
	dg_gets ( &num2, iret );
	if ( *iret != 0 ) return;

        /*
         *	Compute the magnitude of the vector.
         */
	dg_putv ( &numu, &numv, iret );
	if ( *iret != 0 ) return;
	dv_mag ( iret );
	if ( *iret != 0 ) return;

        /*
         *	Get the magnitude.
         */
	dg_gets ( &nmag, iret );
	if ( *iret != 0 ) return;

        /*
         *	Get a new vector.
         */
	dg_nxtv ( &nu, &nv, iret );
	if ( *iret != 0 ) return;

        dg_getg ( &nu, &gru, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nv, &grv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numu, &grnumu, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numv, &grnumv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &num1, &grnum1, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &num2, &grnum2, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &nmag, &grmag, &kxd, &kyd, &ksub1, &ksub2, iret );

        /*
         *	Check all of the grid points.
         */
	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    if ( ERMISS ( grmag[i] ) || ERMISS ( grnum1[i] ) ||
     	 	 ERMISS ( grnum2[i] ) ) {
		gru[i] = RMISSD;
		grv[i] = RMISSD;
            }
	    else {
		if ( ( (grmag[i] > grnum1[i] ) &&
                      (grmag[i] < grnum2[i] ) ) ||
     		     ( (grmag[i] < grnum1[i] ) && 
     		       (grmag[i] > grnum2[i] ) ) ) {
		    gru[i] = grnumu[i];
		    grv[i] = grnumv[i];
		}
		else {
		    gru[i] = RMISSD;
		    grv[i] = RMISSD;
		}
            }

	}

        /*
         *	Make a name of the form 'VBTW'//V//S and 
         *	update both grid headers; update the stack.
         */
	dg_updv ( "VBTW", &nu, &nv, &numu, &num1, iret );
	dg_putv ( &nu, &nv, iret );
	dg_esub  ( &nu, &nv, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
