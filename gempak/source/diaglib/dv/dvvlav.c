#include "dv.h"

void dv_vlav ( int *iret )
/************************************************************************
 * dv_vlav								*
 *									*
 * This subroutine computes VLAV, the layer average for a vector:	*
 *									*
 *     VLAV ( V ) = [ ( u (level1) + u (level2) ) / 2.,			*
 *                    ( v (level1) + v (level2) ) / 2. ]		*
 *									*
 * VLAV generates a vector grid.					*
 *									*
 * dv_vlav  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. Goodman/RDS	12/85						*
 * G. Huffman/GSC	 9/88	Added stack functions			*
 * G. Huffman/GSC	 9/88	Error messages				*
 * K. Brill/GSC		 8/89	Subsetting				*
 * K. Brill/GSC	        10/89   Subsetting				*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * T. Lee/GSC		 5/96   Moved IGDPT outside DO loop		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
	const int       zero = 0;
	int		i, ier, kxd, kyd, ksub1, ksub2;
	int		numu1, numv1, numu2, numv2, numu, numv;
	float		*grnumu1, *grnumv1, *grnumu2, *grnumv2, *gru, *grv;
        float           dgu1, dgv1, dgu2, dgv2;

/*----------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the two (layer) vectors.
         */
	dg_gtvl ( &numu1, &numv1, &numu2, &numv2, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Get a new vector grid number and average the levels.
         */
	dg_nxtv ( &numu, &numv, iret );
	if  ( *iret != 0 ) return;

        dg_getg ( &numu, &gru, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numv, &grv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numu1, &grnumu1, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numv1, &grnumv1, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numu2, &grnumu2, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numv2, &grnumv2, &kxd, &kyd, &ksub1, &ksub2, iret );

	for ( i = ksub1 - 1; i < ksub2; i++ ) {
	    dgu1 = grnumu1[i];
	    dgu2 = grnumu2[i];
	    dgv1 = grnumv1[i];
	    dgv2 = grnumv2[i];

            /*
             *	    U-component.
             */
	    if ( ERMISS (dgu1) || ERMISS (dgu2) )
		gru[i] = RMISSD;
	    else
		gru[i] = ( dgu1 + dgu2 ) / 2.;

            /*
             *	    V-component.
             */
	    if ( ERMISS (dgv1) || ERMISS (dgv2) ) 
		grv[i] = RMISSD;
	    else
		grv[i] = ( dgv1 + dgv2 ) / 2.;
	}

        /*
         *	Make a name of the form 'VLAV'//u1 (u2 is included to get
         *	the second time) and update headers; update stack.
         */
	dg_updv  ( "VLAV", &numu, &numv, &numu1, &numu2, iret );
	dg_putv  ( &numu, &numv, iret );
	dg_esub  ( &numu, &numv, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
