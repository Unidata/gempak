#include "dv.h"

void dv_dot ( int *iret )
/************************************************************************
 * dv_dot								*
 *									*
 * This subroutine computes the dot product of two vectors:		*
 *									*
 *     DOT ( V1, V2 ) = u1 * u2 + v1 * v2				*
 *									*
 * DOT generates a scalar field.					*
 *									*
 * dv_dot  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * I. Graffman/RDS	 7/88	Call to DG_UPDH				*
 * G. Huffman/GSC	 9/88	New stack functions			*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. Brill/GSC		 8/89   Subsetting				*
 * K. Brill/GSC		10/89   Subsetting				*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * T. Lee/GSC		 5/96	Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 ************************************************************************/
{
        int             i, kxd, kyd, ksub1, ksub2, ier, zero=0;
	int		num, numu1, numv1, numu2, numv2;
	float		*grnum, *gru1, *grv1, *gru2, *grv2;
	float		du1, dv1, du2, dv2;

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
         *	Get a new grid number and compute the dot product.
         */
	dg_nxts ( &num, iret );
	if  ( *iret != 0 ) return;

        dg_getg ( &num, &grnum, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numu1, &gru1, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numv1, &grv1, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numu2, &gru2, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numv2, &grv2, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    du1 = gru1[i];
	    dv1 = grv1[i];
	    du2 = gru2[i];
	    dv2 = grv2[i];
	    if  ( ERMISS (du1) || ERMISS (dv1) ||
     		  ERMISS (du2) || ERMISS (dv2) )
		grnum[i] = RMISSD;
	    else
		grnum[i] = du1 * du2  +  dv1 * dv2;
	    
	}

        /*
         *	Make a name of the form 'DOT'//u1//u2 and update header;
         *	update stack.
         */
	dg_updh  ( "DOT", &num, &numu1, &numu2, iret );
	dg_puts  ( &num, iret );
	dg_esub  ( &num, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
