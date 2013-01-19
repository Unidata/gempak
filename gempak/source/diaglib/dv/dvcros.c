#include "dv.h"

void dv_cros ( int *iret )
/************************************************************************
 * dv_cros								*
 *									*
 * This subroutine computes the cross product of two horizontal		*
 * vectors:								*
 *									*
 *     CROS ( V1, V2 ) = u1 * v2 - v1 * u2				*
 *									*
 * CROS generates a scalar field which is the magnitude of the		*
 * resulting upward pointing vector.					*
 *									*
 * dv_cros  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * K. Brill/NMC		10/92						*
 * S. Jacobs/EAI	 2/93	Reworded header comment			*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * T. Lee/GSC		 5/96	Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translate from Fortran                  *
 ************************************************************************/
{
        int             i, kxd, kyd, ksub1, ksub2, ier, zero=0;
	int		numu1, numu2, numv1, numv2, num;
	float		*grnumu1, *grnumu2, *grnumv1, *grnumv2, *grnum;
        float           du1, du2, dv1, dv2;

/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the two vectors.
         */
	dg_getv  ( &numu1, &numv1, iret );
	if  ( *iret != 0 ) return;
	dg_getv  ( &numu2, &numv2, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Get a new grid number and compute the cross product.
         */
	dg_nxts ( &num, iret );
	if ( *iret != 0 ) return;

        /*
         *  Get grids
         */
        dg_getg ( &num, &grnum, &kxd, &kyd, &ksub1, &ksub2, iret);
        dg_getg ( &numu1, &grnumu1, &kxd, &kyd, &ksub1, &ksub2, iret);
        dg_getg ( &numu2, &grnumu2, &kxd, &kyd, &ksub1, &ksub2, iret);
        dg_getg ( &numv1, &grnumv1, &kxd, &kyd, &ksub1, &ksub2, iret);
        dg_getg ( &numv2, &grnumv2, &kxd, &kyd, &ksub1, &ksub2, iret);

        /*
         *  get subset
         */
	for ( i = ksub1 - 1; i < ksub2; i++) {
	    du1 = grnumu1[i];
	    dv1 = grnumv1[i];
	    du2 = grnumu2[i];
	    dv2 = grnumv2[i];
	    if  ( ERMISS (du1) || ERMISS (dv1) || ERMISS (du2) || ERMISS (dv2) )
		grnum[i] = RMISSD;
	    else
		grnum[i] = du1 * dv2  -  dv1 * du2;
	    
	}

        /*
         *	Make a name of the form 'CROS'//u1//u2 and update header;
         *	update stack.
         */
	dg_updh  ( "CROS", &num, &numu1, &numu2, iret );
	dg_puts  ( &num, iret );
	dg_esub  ( &num, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
