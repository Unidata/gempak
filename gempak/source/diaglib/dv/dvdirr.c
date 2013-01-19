#include "dv.h"

void dv_dirr  ( int *iret )
/************************************************************************
 * dv_dirr								*
 *									*
 * This subroutine returns the direction of a vector relative to	*
 * the grid:								*
 *									*
 *     DIRR ( V ) = PD_DRCT ( u, v )					*
 *									*
 * dv_dirr  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					As for DG_GETV			*
 **									*
 * Log:									*
 * M. desJardins/GSFC	10/85						*
 * I. Graffman/RDS	 7/88	Call to DG_UPDH				*
 * G. Huffman/GSC	 9/88	New stack functions; Error messages	*
 * M. desJardins/GSFC	 4/89	Added grid relative functions		*
 * M. desJardins/GSFC	 7/89	Added PA subroutines			*
 * M. desJardins/GSFC	 8/89	PA to PD subroutines			*
 * T. Lee/GSC		 4/96   Single dimension for dgg		*
 * K. Brill/HPC		 1/02	CALL DG_SSUB and DG_ESUB		*
 * K. Brill/HPC		11/02	Eliminate use of the SUBA logical array	*
 * S. Gilbert/NCEP	11/05	Translation from Fortran                *
 ************************************************************************/
{
	int 		ier, zero=0, kxd, kyd, kxyd, ksub1, ksub2;
	int 		numu, numv, numout;
        float           *gru, *grv, *grout;

/*------------------------------------------------------------------------*/
	*iret = 0;
	dg_ssub ( iret );

        /*
         *	Get the vector grid.
         */
	dg_getv  ( &numu, &numv, iret );
	if  ( *iret != 0 ) return;

        /*
         *	Get a new grid index and compute the direction from U and V.
         */
	dg_nxts  ( &numout, iret );
	if  ( *iret != 0 ) return;

        dg_getg ( &numu, &gru, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numv, &grv, &kxd, &kyd, &ksub1, &ksub2, iret );
        dg_getg ( &numout, &grout, &kxd, &kyd, &ksub1, &ksub2, iret );

	kxyd = kxd * kyd;
	pd_drct  ( gru, grv, &kxyd, grout, &ier );

        /*
         *	Make a name of the form 'DIR'//u and update header;
         *	update the stack.
         */
	dg_updh  ( "DIR", &numout, &numu, &zero, iret );
	dg_puts  ( &numout, iret );
	dg_esub  ( &numout, &zero, &zero, &zero, &ier );
	if ( ier != 0 ) *iret = ier;

	return;
}
