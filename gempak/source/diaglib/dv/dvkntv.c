#include "dv.h"

void dv_kntv  ( int *iret )
/************************************************************************
 * dv_kntv								*
 *									*
 * This subroutine computes the vector KNTV in knots from a vector in	*
 * meters/second:							*
 *									*
 *     KNTV ( V ) = [ PD_MSKN (u), PD_MSKN (v) ]			*
 *									*
 * KNTV generates a vector grid.					*
 *									*
 * dv_kntv ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. Goodman/RDS	11/85						*
 * M. desJardins/GSFC	 5/88	Added new stack functions		*
 * G. Huffman/GSC	 9/88	Error messages				*
 * M. desJardins/GSFC	 7/89	Added PA subroutines			*
 * M. desJardins/GSFC	 8/89	PA to PD subroutines			*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
        int             ier, kxd, kyd, kxyd, ksub1, ksub2, zero=0;
	int		numu, numv, nkntu, nkntv;
	float		*gru, *grv, *grkntu, *grkntv;

/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the input vector.
         */
	dg_getv  ( &numu, &numv, iret );
	if  ( *iret != 0 )  return;

        /*
         *	Find space for a new vector.
         */
	dg_nxtv  ( &nkntu, &nkntv, iret );
	if  ( *iret != 0 )  return;

        dg_getg( &numu, &gru, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg( &numv, &grv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg( &nkntu, &grkntu, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg( &nkntv, &grkntv, &kxd, &kyd, &ksub1, &ksub2, iret );
	kxyd = kxd * kyd;

        /*
         *	Convert the vector from meters/second to knots
         */
	pd_mskn  ( gru, &kxyd, grkntu, &ier );
	pd_mskn  ( grv, &kxyd, grkntv, &ier );

        /*
         *	Make a name of the form 'KNTV'//u and update the grid headers;
         *	update the stack.
         */
	dg_updv  ( "KNTV", &nkntu, &nkntv, &numu, &zero, iret );
	dg_putv  ( &nkntu, &nkntv, iret );
	dg_esub  ( &nkntu, &nkntv, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
